{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}


-- TODO:
--   save stats for wind speed and direction to generate wind rose?
--   create separate executables for the different stages so that we have something like:
--      * main executable that does everything
--      * load CSV and turn into AwSlCombined and save to CSV
--      * fill in missing values for <5 hour gaps (should this reuse previous code, or load from CSV?)
--      * fill in missing values for X hour gaps etc.


module Main where

import Control.Lens                         (Lens', (^.), (.~), (&), (+~))
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Csv.Streaming                   (Records)
import Data.List                            (groupBy, foldl1')
import Data.Maybe                           (fromJust)
import Data.Text                            (Text, unpack)
import Data.Time.Clock                      (diffUTCTime)
import Data.Time.Lens                       (flexDT, minutes)
import Data.Time.LocalTime                  (LocalTime, localTimeToUTC, utc)
import System.Directory                     (doesFileExist)
import System.Environment                   (getArgs)
import System.FilePath.Find                 (find, always, (~~?), fileName)

import Tmy.OneMinSolar.Functions
import Tmy.OneMinSolar.Types
import Tmy.Common
import Tmy.Csv

-- import Debug.Trace


main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "No files specified."
        else do
            -- read the CSV files, typed for clarity
            sitesMeta <- mapM readCsv args :: IO [Records OneMinSolarSite]
            -- zip the list of records with the CSV filename for later, typed for clarity
            let sitesAndFiles = zip args sitesMeta :: [(String, Records OneMinSolarSite)]
            -- partially apply the filename of the CSV file to the processing function
            mapM_ (\(fn,recs) -> mapRecords_ (processSingleSite fn) recs) sitesAndFiles

            -- DEBUG
            {-
            forM_ sitesAndFiles $ \(fn,recs) -> do
                putStrLn ("CSV file: " ++ fn)
                mapRecords_ print recs
            -}


processSingleSite :: FilePath -> OneMinSolarSite -> IO ()
processSingleSite fn s = do
    let csvDir = (reverse . dropWhile ('/' /=) . reverse) fn
        stationNum = (unpack . unSpaced . bomStationNum) s
        awGlob = awPref ++ stationNum ++ globSuff
        slGlob = slPref ++ stationNum ++ globSuff
        newCsv = stationNum ++ "_averaged.csv"
    awFiles <- find always (fileName ~~? awGlob) csvDir
    slFiles <- find always (fileName ~~? slGlob) csvDir
    -- read all data into two long lists of records to avoid BoM month end/begin mismatch
    awRecs <- mapM readIndexedCsv awFiles
    slRecs <- mapM readCsv slFiles
    fnExists <- doesFileExist newCsv
    let encOpts = defaultEncodeOptions {encIncludeHeader = not fnExists}
        -- concatenate all records from all files as aw and sl file timestamps do not line up
        awRecsList = concatRecs awRecs
        slRecsList = concatRecs slRecs
        -- turn the records into Stat recs, this filters out aw values by quality
        awStats = map awToStat awRecsList
        slStats = map slToStat slRecsList
        -- fill in missing data
        awInfilled = awProcess infill awStats
        slInfilled = slProcess infill slStats
        -- check the filled in data
        awChecked = awProcess check awInfilled
        slChecked = slProcess check slInfilled
        -- group into hours
        awStatGroups = groupBy (hourGrouper awLTimeSt) awChecked
        slStatGroups = groupBy (hourGrouper slLTimeSt) slChecked
        -- aggregate 1-minute records to hours
        awFolded = map (foldl1' awAggr) awStatGroups
        slFolded = map (foldl1' slAggr) slStatGroups
        -- !_ = traceShowId ((take 5) awFolded)
        -- !_ = traceShowId ((take 5) slFolded)
        -- combine 1-hour aw and sl records
        merged = mergeWith awLTimeSt slLTimeSt AwSlCombined awFolded slFolded
    if null merged
        then putStrLn ("No records found for station " ++ show stationNum)
        else do
            putStrLn ("Processing " ++ show newCsv)
            BL.appendFile newCsv (encodeDefaultOrderedByNameWith encOpts merged)


data FieldType ftype = FieldType
    { mkValue  :: Double1Dec -> ftype
    , getValue :: ftype      -> Double1Dec
    }


ftStat :: FieldType (Stat Double1Dec)
ftStat = FieldType
    { mkValue  = \v -> mkFillStat v v v
    , getValue = statMean
    }


ftMean :: FieldType (Mean Double1Dec)
ftMean = FieldType
    { mkValue  = mkFillMean
    , getValue = mMean
    }


type Processor = (Show a, Show b) => Processing a -> (Lens' a (Maybe b)) -> FieldType b -> [a] -> [a]


awProcess :: Processor -> [AwStats] -> [AwStats]
awProcess p xs =
    ( f awAirTempSt         ftStat
    . f awWetBulbTempSt     ftStat
    . f awDewPointTempSt    ftStat
    . f awRelHumidSt        ftStat
    . f awWindSpeedSt       ftStat
    . f awVisibilitySt      ftMean
    . f awMslPressSt        ftMean
    . f awStationLvlPressSt ftMean
    . f awQnhPressSt        ftMean
    ) xs
    where
        f :: Show b => Lens' AwStats (Maybe b) -> FieldType b -> [AwStats] -> [AwStats]
        f = p awStatsP


slProcess :: Processor -> [SlStats] -> [SlStats]
slProcess p xs =
    ( f slGhiSt    ftStat
    . f slDniSt    ftStat
    . f slDiffSt   ftStat
    . f slTerrSt   ftStat
    . f slDhiSt    ftStat
    . f slZenithSt ftMean
    ) xs
    where
        f :: Show b => Lens' SlStats (Maybe b) -> FieldType b -> [SlStats] -> [SlStats]
        f = p slStatsP


-- | Check that the infilling of values has succeeded and there are no more gaps
--   of data shorter than the infill max gap length.
check :: (Show a, Show b)
      => Processing a
      -> (Lens' a (Maybe b))
      -> FieldType b
      -> [a]
      -> [a]
check pr@(Processing{..}) f _ ss = go ss where
    go as@(a:xs) =
        -- check if a has a value for this time
        case a ^. f of
            Nothing -> a : go xs -- skip until we find a value for the field
            Just _  ->
                case minutesUntil pr f (lTime a) xs of
                    Nothing -> as
                    Just ((mins, b)) ->
                        if mins > 1 && isLessThan5Hours mins
                            then error ("Found a gap of " ++ show mins
                                        ++ " minutes, shorter than the minimum 300. From "
                                        ++ show (lTime a) ++ " to " ++ show (lTime b)
                                        ++ ".\n\nThe two records are:\n\n"++ show a ++ "\n\n" ++ show b
                                        ++ "\n\na: " ++ show (a ^. f) ++ "\nb: " ++ show (b ^. f))
                            else a : go xs
    go [] = []


data Processing recType = Processing
    { lTime    :: recType -> LocalTime
    , stNum    :: recType -> Text
    , mkEmpty  :: Text    -> LocalTime -> recType
    }


awStatsP :: Processing AwStats
awStatsP = Processing
    { lTime   = unLTime . awLTimeSt
    , stNum   = awStationNumSt
    , mkEmpty = mkAwStats
    }


slStatsP :: Processing SlStats
slStatsP = Processing
    { lTime   = unLTime . slLTimeSt
    , stNum   = slStationNumSt
    , mkEmpty = mkSlStats
    }


infill :: Processing a
       -> (Lens' a (Maybe b))
       -> FieldType b
       -> [a]
       -> [a]
infill pr@(Processing{..}) f ft as@(a:xs) =
    -- check that we have a record with a value for this field
    case a ^. f of
        -- if not then we must be at the start of the list, so iterate until we find one
        Nothing -> a : infill pr f ft xs
        Just _  ->
            -- if we do then check how long until the next value for this field
            case minutesUntil pr f (lTime a) xs of
                -- if not then we must be at the end of the list
                Nothing -> as
                Just ((mins, b)) ->
                    if mins > 1 && isLessThan5Hours mins
                        then let xs' = linearlyInterpolate pr f ft mins a b xs
                             in  a : infill pr f ft xs'
                        else a : infill pr f ft xs
infill _ _ _ [] = []


isLessThan5Hours :: Int -> Bool
isLessThan5Hours mins = mins < 300


minDiff :: LocalTime -> LocalTime -> Int
minDiff a b = round (diffUTCTime (localTimeToUTC utc a) (localTimeToUTC utc b) / 60)


-- | Find the number of minutes as well as the record that has a Just value for a given field
minutesUntil :: Processing a
             -> (Lens' a (Maybe b))
             -> LocalTime
             -> [a]
             -> Maybe (Int, a)
minutesUntil (Processing{..}) f lt xs = go xs where
    -- check if the field we are interested in has a value
    go (a:as) = case a ^. f of
                    -- if it doesn't, then increment and keep looking
                    Nothing -> go as
                    -- if the field has a value then return the minutes difference and the record
                    Just _  -> Just (minDiff (lTime a) lt, a)
    go [] = Nothing


linearlyInterpolate :: Processing a
                    -> (Lens' a (Maybe b))
                    -> FieldType b
                    -> Int
                    -> a
                    -> a
                    -> [a]
                    -> [a]
linearlyInterpolate _ _ _ 0 _ _ xs = xs
linearlyInterpolate (Processing{..}) f (FieldType{..}) num a b xs' = go 1 xs' where
    lt x       = lTime x                        -- get the LocalTime from an AwStats
    addMin x m = lt x & flexDT.minutes +~ m     -- add minutes to a LocalTime
    va         = getValue (fromJust (a ^. f))   -- the mean value of the field for a
    vb         = getValue (fromJust (b ^. f))   -- the mean value of the field for b
    vincr      = (vb - va) / fromIntegral (num) -- the linear increment
    val n      = va + (vincr * fromIntegral n)  -- the new mean of the nth linearly interpolated record
    go _ []    = []
    go n ss@(x:xs)
        -- we've done as many infills as we needed, all done
        | n >= num           = ss
        -- found a record with the right time, modify with new stat
        | lt x == addMin a n = (x & f .~ Just (mkValue (val n))) : go (n+1) xs
        -- no record with the right time, make one and set the stat
        | otherwise          = (mkEmpty (stNum a) (addMin a n) & f .~ Just (mkValue (val n))) : go (n+1) ss


mkAwStats :: Text -> LocalTime -> AwStats
mkAwStats stNum lt = AwStats stNum (LTime lt)
                        Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing


mkSlStats :: Text -> LocalTime -> SlStats
mkSlStats stNum lt = SlStats stNum (LTime lt)
                        Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
