{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- TODO:
--   combine wind direction using vector math
--     save stats for wind speed and direction to generate wind rose?
--   filling in missing data?
--   round to 1 decimal

module Main where

import qualified Data.ByteString.Lazy as BL
import Control.Applicative                  ((<$>))
import Data.Csv
import Data.Csv.Streaming                   (Records(Nil))
import Data.Maybe                           (fromMaybe)
import Data.List                            (groupBy, foldl1')
import Data.Semigroup                       (Semigroup, Sum(..), Min(..), Max(..), (<>))
import Data.Text                            (unpack)
import Data.Time.LocalTime                  (LocalTime(..), TimeOfDay(..), localTimeOfDay, localDay, todHour)
import System.Directory                     (doesFileExist)
import System.Environment                   (getArgs)
import System.FilePath.Find                 (find, always, (~~?), fileName)

import Tmy.OneMinSolarTypes
import Tmy.Csv

import Debug.Trace


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
        awRecsList = recsAsList awRecs (Nil Nothing BL.empty)
        slRecsList = recsAsList slRecs (Nil Nothing BL.empty)
        -- group and compute stats for aw and sl separately
        awGroups = groupBy (hourGrouper awLocalTime) awRecsList
        slGroups = groupBy (hourGrouper slLocalTime) slRecsList
        -- AutoWeatherObs to StatAutoWeatherObs, filter out poor quality
        awStatGroups = map (map awToStat) awGroups
        slStatGroups = map (map slToStat) slGroups
        -- aggregate 1-minute records to hours
        awFolded = map (foldl1' awAggrToHour) awStatGroups
        slFolded = map (foldl1' slAggrToHour) slStatGroups
        -- !_ = traceShowId ((take 5) awFolded)
        -- !_ = traceShowId ((take 5) slFolded)
        -- combine 1-hour aw and sl records
        merged = mergeWith awLocalTimeSt slLocalTimeSt AwSlCombined awFolded slFolded
    if null merged
        then putStrLn ("No records found for station " ++ show stationNum)
        else do
            putStrLn ("Processing " ++ show newCsv)
            BL.appendFile newCsv (encodeDefaultOrderedByNameWith encOpts merged)


combine :: Semigroup b => (a -> b) -> a -> a -> b
combine f a b = f a <> f b


awAggrToHour :: AwStats -> AwStats -> AwStats
awAggrToHour a b =
    AwStats
        { awStationNumSt      = awStationNumSt   a
        , awLocalTimeSt       = floorMinute (awLocalTimeSt a)
        , awLocalStdTimeSt    = awLocalStdTimeSt a
        , awUtcTimeSt         = awUtcTimeSt      a
        , awAirTempSt         = combine awAirTempSt         a b
        , awWetBulbTempSt     = combine awWetBulbTempSt     a b
        , awDewPointTempSt    = combine awDewPointTempSt    a b
        , awRelHumidSt        = combine awRelHumidSt        a b
        , awWindSpeedSt       = combine awWindSpeedSt       a b
        , awPrecipSinceLastSt = combine awPrecipSinceLastSt a b
        , awWindDirSt         = Just 0 -- TODO: vector math
        , awVisibilitySt      = combine awVisibilitySt      a b
        , awMslPressSt        = combine awMslPressSt        a b
        , awStationLvlPressSt = combine awStationLvlPressSt a b
        , awQnhPressSt        = combine awQnhPressSt        a b
        }


slAggrToHour :: SlStats -> SlStats -> SlStats
slAggrToHour a b =
    SlStats
        { slStationNumSt      = slStationNumSt              a
        , slLocalTimeSt       = floorMinute (slLocalTimeSt  a)
        , slGhiSt             = combine slGhiSt             a b
        , slDniSt             = combine slDniSt             a b
        , slDiffSt            = combine slDiffSt            a b
        , slTerrSt            = combine slTerrSt            a b
        , slDhiSt             = combine slDhiSt             a b
        , slSunshineSecs96St  = combine slSunshineSecs96St  a b
        , slSunshineSecs120St = combine slSunshineSecs120St a b
        , slSunshineSecs144St = combine slSunshineSecs144St a b
        , slZenithSt          = combine slZenithSt          a b
        }


awToStat :: AutoWeatherObs -> AwStats
awToStat a =
    AwStats
        { awStationNumSt      = unSpaced (awStationNum a)
        , awLocalTimeSt       = awLocalTime    a
        , awLocalStdTimeSt    = awLocalStdTime a
        , awUtcTimeSt         = awUtcTime      a
        , awAirTempSt         = maybeQualStat awAirTempQual      awAirTemp      awAirTempMax      awAirTempMin      a
        , awWetBulbTempSt     = maybeQualStat awWetBulbTempQual  awWetBulbTemp  awWetBulbTempMax  awWetBulbTempMin  a
        , awDewPointTempSt    = maybeQualStat awDewPointTempQual awDewPointTemp awDewPointTempMax awDewPointTempMin a
        , awRelHumidSt        = maybeQualStat awRelHumidQual     awRelHumid     awRelHumidMax     awRelHumidMin     a
        , awWindSpeedSt       = maybeQualStat awWindSpeedQual    awWindSpeed    awWindGustMax     awWindSpeedMin    a
        , awPrecipSinceLastSt = Sum <$> qFilter awPrecipQual  awPrecipSinceLast a
        , awWindDirSt         = Just 0 -- TODO: vector math
        , awVisibilitySt      = mkSumCount <$> qFilter awVisibilityQual      awVisibility      a
        , awMslPressSt        = mkSumCount <$> qFilter awMslPressQual        awMslPress        a
        , awStationLvlPressSt = mkSumCount <$> qFilter awStationLvlPressQual awStationLvlPress a
        , awQnhPressSt        = mkSumCount <$> qFilter awQnhPressQual        awQnhPress        a
        }


slToStat :: SolarRadiationObs -> SlStats
slToStat a =
    SlStats
        { slStationNumSt      = unSpaced (slStationNum a)
        , slLocalTimeSt       = slLocalTime a
        , slGhiSt             = maybeStat slGhiMean  slGhiMax  slGhiMin  a
        , slDniSt             = maybeStat slDniMean  slDniMax  slDniMin  a
        , slDiffSt            = maybeStat slDiffMean slDiffMax slDiffMin a
        , slTerrSt            = maybeStat slTerrMean slTerrMax slTerrMin a
        , slDhiSt             = maybeStat slDhiMean  slDhiMax  slDhiMin  a
        , slSunshineSecs96St  = Sum <$> unSpaced (slSunshineSecs96  a)
        , slSunshineSecs120St = Sum <$> unSpaced (slSunshineSecs120 a)
        , slSunshineSecs144St = Sum <$> unSpaced (slSunshineSecs144 a)
        , slZenithSt          = mkSumCount <$> unSpaced (slZenith a)
        }


maybeStat :: (a -> (Spaced (Maybe b)))
          -> (a -> (Spaced (Maybe b)))
          -> (a -> (Spaced (Maybe b)))
          -> a
          -> Maybe (Stat b)
maybeStat meanF maxF minF a =
    case maybeMean of
        Just mean -> Just (mkStat mean (fromMaybe mean maybeMax) (fromMaybe mean maybeMin))
        Nothing   -> Nothing
    where
        maybeMean = unSpaced (meanF a)
        maybeMax  = unSpaced (maxF a)
        maybeMin  = unSpaced (minF a)


maybeQualStat :: (a -> Spaced Char)
              -> (a -> (Spaced (Maybe b)))
              -> (a -> (Spaced (Maybe b)))
              -> (a -> (Spaced (Maybe b)))
              -> a
              -> Maybe (Stat b)
maybeQualStat meanQf meanF maxF minF a =
    case qFilter meanQf meanF a of
        Just _  -> maybeStat meanF maxF minF a
        Nothing -> Nothing


qFilter :: (a -> Spaced Char)
        -> (a -> (Spaced (Maybe b)))
        -> a
        -> Maybe b
qFilter qf vf a =
    if (unSpaced (qf a)) `elem` "YNSF"
        then unSpaced (vf a)
        else Nothing


mkStat :: a -> a -> a -> Stat a
mkStat smean smax smin = Stat smean (Max smax) (Min smin) 1


mkSumCount :: a -> SumCount a
mkSumCount a = SumCount a 1


hourGrouper :: (a -> LocalTime) -> a -> a -> Bool
hourGrouper f a b = floorMinute (f a) == floorMinute (f b)


floorMinute :: LocalTime -> LocalTime
floorMinute a = LocalTime (localDay a) (TimeOfDay (todHour (localTimeOfDay a)) 0 0)


mergeWith :: Ord c => (a -> c)
                   -> (b -> c)
                   -> (Maybe a -> Maybe b -> r)
                   -> [a] -> [b] -> [r]
mergeWith fa fb comb xs ys = go xs ys where
    go [] [] = []
    go [] bs = map (comb Nothing)      (map Just bs)
    go as [] = map (flip comb Nothing) (map Just as)
    go aas@(a:as) bbs@(b:bs) = case compare (fa a) (fb b) of
        LT -> comb (Just a) Nothing  : go as  bbs
        EQ -> comb (Just a) (Just b) : go as  bs
        GT -> comb Nothing  (Just b) : go aas bs

