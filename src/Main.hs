{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- TODO:
-- either:
--   think about quality columns
--   start averaging values


module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Csv.Streaming                   (Records(Cons, Nil))
import Data.Either                          (partitionEithers)
import Data.Maybe                           (fromJust)
import Data.List                            (groupBy)
import Data.Semigroup                       (Min(..), Max(..))
import Data.Text                            (Text, unpack, isInfixOf)
import Data.Time.LocalTime                  (LocalTime(..), TimeOfDay(..), localTimeOfDay, todMin, localDay, todHour)
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
    mapM_ (processCsvPair newCsv) (zip awFiles slFiles)


processCsvPair :: FilePath -> (FilePath, FilePath) -> IO ()
processCsvPair fn t@(aw, sl) = do
    putStrLn ("Processing " ++ show t)
    -- typed for clarity
    awRecs <- readIndexedCsv aw :: IO (Records AutoWeatherObs)
    slRecs <- readCsv sl :: IO (Records SolarRadiationObs)
    fnExists <- doesFileExist fn
    let encOpts = defaultEncodeOptions {encIncludeHeader = not fnExists}
        -- Records a -> [Either String a]
        -- partitionEithers: [Either String a] -> ([String], [a])
        (awErrs, awList) = partitionEithers (recsAsList awRecs)
        (slErrs, slList) = partitionEithers (recsAsList slRecs)
        -- group and compute stats for aw and sl separately
        awGroups = groupBy (hourGrouper awLocalTime) awList
        lwGroups = groupBy (hourGrouper slLocalTime) slList
        -- AutoWeatherObs to StatAutoWeatherObs, filter out shit quality
        awStatGroups = map (map awoToStat) awGroups
        !_ = traceShowId (map (map awAirTempSt) (take 3 awStatGroups))
        -- awFolded = map (foldl1' awAggrToHour) awGroups
        -- same for sl
        -- combine 1-hour aw and sl records
            -- [awFolded] -> [slFolded] -> [awSlCombined]
        -- combined = mergeWith awLocalTime slLocalTime CombinedAwSlObs awFolded slFolded

        combined = combineAwSl awRecs slRecs -- DEBUG to remove
        (lefts, rights) = partitionEithers combined
        filtered = filter zeroMinutes rights -- DEBUG to remove
    -- mapM_ putStrLn lefts
    mapM_ putStrLn awErrs
    mapM_ putStrLn slErrs
    BL.appendFile fn (encodeDefaultOrderedByNameWith encOpts filtered)


{-
awAggrToHour :: AutoWeatherObs -> AutoWeatherObs -> AutoWeatherObs
awAggrToHour a b =
    AutoWeatherObs
        { awStationNum=awStationNum a
        , localTime a
        , awPrecipSinceLast+awPrecipSinceLast
        , temp, min, max, count
        , ...
    }
-}
-- func Spaced (Maybe a) -> Option (Maybe a)


awoToStat :: AutoWeatherObs -> AwoStats
awoToStat a =
    AwoStats
        { awStationNumSt      = unSpaced (awStationNum a)
        , awLocalTimeSt       = awLocalTime a
        , awLocalStdTimeSt    = awLocalStdTime a
        , awUtcTimeSt         = awUtcTime a
        , awAirTempSt         = maybeStat awAirTempQual      awAirTemp      awAirTempMax      awAirTempMin      a
        , awWetBulbTempSt     = maybeStat awWetBulbTempQual  awWetBulbTemp  awWetBulbTempMax  awWetBulbTempMin  a
        , awDewPointTempSt    = maybeStat awDewPointTempQual awDewPointTemp awDewPointTempMax awDewPointTempMin a
        , awRelHumidSt        = maybeStat awRelHumidQual     awRelHumid     awRelHumidMax     awRelHumidMin     a
        , awPrecipSinceLastSt = qFilter awPrecipQual          awPrecipSinceLast a
        , awWindSpeedSt       = qFilter awWindSpeedQual       awWindSpeed a
        , awWindSpeedMinSt    = qFilter awWindSpeedMinQual    awWindSpeedMin a
        , awWindDirSt         = qFilter awWindDirQual         awWindDir a
        , awWindGustMaxSt     = qFilter awWindGustMaxQual     awWindGustMax a
        , awVisibilitySt      = qFilter awVisibilityQual      awVisibility a
        , awMslPressSt        = qFilter awMslPressQual        awMslPress a
        , awStationLvlPressSt = qFilter awStationLvlPressQual awStationLvlPress a
        , awQnhPressSt        = qFilter awQnhPressQual        awQnhPress a
        }


maybeStat :: (a -> Text)
          -> (a -> (Spaced (Maybe b)))
          -> (a -> (Spaced (Maybe b)))
          -> (a -> (Spaced (Maybe b)))
          -> a
          -> Maybe (Stat b)
maybeStat valQf valF maxF minF a =
    case qFilter valQf valF a of
        Just v  -> Just (mkStat v (fromJust (unSpaced (maxF a))) (fromJust (unSpaced (minF a))))
        Nothing -> Nothing


qFilter :: (a -> Text)
        -> (a -> (Spaced (Maybe b)))
        -> a
        -> Maybe b
qFilter qf vf a =
    if (qf a) `isInfixOf` "YNSF"
        then unSpaced (vf a)
        else Nothing


mkStat :: a -> a -> a -> Stat a
mkStat smean smax smin = Stat smean (Max smax) (Min smin) 1


hourGrouper :: (a -> LocalTime) -> a -> a -> Bool
hourGrouper f a b = floorMinute (f a) == floorMinute (f b)


floorMinute :: LocalTime -> LocalTime
floorMinute a = LocalTime (localDay a) (TimeOfDay (todHour (localTimeOfDay a)) 0 0)


{-
mergeWith :: Ord c => (a -> c)
                   -> (b -> c)
                   -> (Maybe a -> Maybe b -> r)
                   -> [a] -> [b] -> [r]
-}


zeroMinutes :: CombinedAwSlObs -> Bool
zeroMinutes c = (todMin . localTimeOfDay . awLocalStdTime . awRecord) c == 00


combineAwSl :: Records AutoWeatherObs -> Records SolarRadiationObs -> [Either String CombinedAwSlObs]
combineAwSl (Cons a rs)     (Cons b rs2)     = do
    case comb CombinedAwSlObs a b of
        l@(Left _) -> l : combineAwSl rs rs2
        r@(Right (CombinedAwSlObs aw sl)) ->
            if (awLocalTime aw) == (slLocalTime sl)
                -- if we have matching localTimes then add this CombinedAwSlObs to the list
                then r : combineAwSl rs rs2
                else if (awLocalTime aw) < (slLocalTime sl)
                        -- if we have missing parallel times then log and discard the partial record
                        then Left ("Discarded lonely weather obs at awLocalTime: " ++ (show . awLocalTime) aw)
                                : combineAwSl rs (Cons (Right sl) rs2)
                        else Left ("Discarded lonely solar obs at slLocalTime: " ++ (show . slLocalTime) sl)
                                : combineAwSl (Cons (Right aw) rs) rs2
combineAwSl (Nil Nothing _) (Cons _ _)       = [Left ("Unprocessed solar obs")]
combineAwSl (Cons _ _)      (Nil Nothing _)  = [Left ("Unprocessed weather obs")]
combineAwSl (Nil (Just e) _) _               = [Left ("Failed processing weather CSV file: " ++ e)]
combineAwSl _               (Nil (Just e) _) = [Left ("Failed processing solar CSV file: " ++ e)]
combineAwSl (Nil Nothing _) (Nil Nothing _)  = [] -- success! both ended at the same time


comb :: (a -> b -> c) -> Either String a -> Either String b -> Either String c
comb f (Right a) (Right b) = Right (f a b)
comb _ (Left a) (Left b) = Left ("Two errors: '" ++ a ++ "' and '" ++ b ++ "'")
comb _ (Left a) _ = Left ("aw side: " ++ a)
comb _ _ (Left b) = Left ("sl side: " ++ b)
