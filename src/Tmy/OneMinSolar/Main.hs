{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- TODO:
--   save stats for wind speed and direction to generate wind rose?
--   count filled in data separately

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
        awInfilled = check awAirTempSt (infill awAirTempSt awStats)
        -- awInfilled = check awAirTempSt awStats
        -- slInfilled = infill slStats
        -- group into hours
        awStatGroups = groupBy (hourGrouper awLTimeSt) awInfilled
        slStatGroups = groupBy (hourGrouper slLTimeSt) slStats
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


-- TODO: test that this errors on 5+ hour gaps
check :: (Lens' AwStats (Maybe (Stat Double1Dec))) -> [AwStats] -> [AwStats]
check f ss = go ss where
    go (a:b:xs) =
        case a ^. f of
            Nothing -> a : go (b:xs) -- skip until we find a value for the field
            Just _  ->
                case b ^. f of
                    Nothing ->
                        let lta = (unLTime (awLTimeSt a))
                            ltb = (unLTime (awLTimeSt b))
                            mins = minDiff ltb lta
                        in  if lessThan5hours mins
                                then error ("Found a gap of " ++ show mins
                                            ++ " minutes, shorter than the minimum 300. From "
                                            ++ show lta ++ " to " ++ show ltb ++ ".")
                                else a : go (b:xs)
                    Just _  -> a : go (b:xs)
    go xs = xs


infill :: (Lens' AwStats (Maybe (Stat Double1Dec))) -> [AwStats] -> [AwStats]
infill f as@(a:xs) =
    case minutesUntil (unLTime (awLTimeSt a)) f xs of
        Nothing -> as
        Just ((mins, b)) ->
            if mins > 0 && lessThan5hours mins
                then let xs' = linearlyInterpolate f mins a b xs
                     in  a : infill f xs'
                else a : infill f xs
infill _ [] = []


lessThan5hours :: Int -> Bool
lessThan5hours mins = mins < 300


minDiff :: LocalTime -> LocalTime -> Int
minDiff a b = round (diffUTCTime (localTimeToUTC utc a) (localTimeToUTC utc b) / 60)


-- | Find the number of minutes as well as the record that has a Just value for a given field
minutesUntil :: LocalTime
             -> (Lens' AwStats (Maybe (Stat Double1Dec)))
             -> [AwStats]
             -> Maybe (Int, AwStats)
minutesUntil lt f xs = go xs where
    go (a:as) = case a ^. f of        -- get the field we are interested in
                    Nothing -> go as  -- if it's Nothing, then increment and keep looking
                    Just _  -> Just (minDiff (unLTime (awLTimeSt a)) lt, a)  -- if the field has a value then return the minutes difference and the record
    go [] = Nothing


linearlyInterpolate :: (Lens' AwStats (Maybe (Stat Double1Dec)))
                    -> Int
                    -> AwStats
                    -> AwStats
                    -> [AwStats]
                    -> [AwStats]
linearlyInterpolate _ 0   _ _ xs = xs
linearlyInterpolate f num a b xs' = go 1 xs' where
    lt x       = unLTime (awLTimeSt x)         -- get the LocalTime from an AwStats
    addMin x m = lt x & flexDT.minutes +~ m    -- add minutes to a LocalTime
    va         = statMean (fromJust (a ^. f))  -- the mean value of the field for a
    vb         = statMean (fromJust (b ^. f))  -- the mean value of the field for b
    vincr      = (vb - va) / fromIntegral (num)  -- the linear increment
    val n      = va + (vincr * fromIntegral n) -- the new mean of the nth linearly interpolated record
    stat v     = mkStat v v v                  -- the new Stat value for the field
    go _ []    = []
    go n ss@(x:xs)
        | n >= num           = ss -- we're done
        | lt x == addMin a n = (x & f .~ Just (stat (val n))) : go (n+1) xs -- the next AwStat has the right time, modify with new stat
        | otherwise          = (mkAwStats (awStationNumSt a) (addMin a n) & f .~ Just (stat (val n))) : go (n+1) ss


mkAwStats :: Text -> LocalTime -> AwStats
mkAwStats stNum lt = AwStats stNum (LTime lt)
                        Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing



