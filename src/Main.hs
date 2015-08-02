{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- TODO:
--   aggregate wind direction
--   merge weather and solar into super-record
--   save super record as CSV

module Main where

import qualified Data.ByteString.Lazy as BL
import Control.Applicative                  ((<$>))
import Data.Csv
import Data.Csv.Streaming                   (Records(Cons, Nil))
import Data.Either                          (partitionEithers)
import Data.Maybe                           (fromMaybe)
import Data.List                            (groupBy, foldl1')
import Data.Semigroup                       (Semigroup, Sum(..), Min(..), Max(..), (<>))
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
        slGroups = groupBy (hourGrouper slLocalTime) slList
        -- AutoWeatherObs to StatAutoWeatherObs, filter out poor quality
        awStatGroups = map (map awToStat) awGroups
        slStatGroups = map (map slToStat) slGroups
        -- aggregate 1-minute records to hours
        awFolded = map (foldl1' awAggrToHour) awStatGroups
        slFolded = map (foldl1' slAggrToHour) slStatGroups
        -- combine 1-hour aw and sl records
        merged = mergeWith awLocalTimeSt slLocalTimeSt AwSlCombined awFolded slFolded
        !_ = traceShowId ((take 10 . drop 40) merged)

        combined = combineAwSl awRecs slRecs -- DEBUG to remove
        (lefts, rights) = partitionEithers combined
        filtered = filter zeroMinutes rights -- DEBUG to remove
    -- mapM_ putStrLn lefts
    mapM_ putStrLn awErrs
    mapM_ putStrLn slErrs
    BL.appendFile fn (encodeDefaultOrderedByNameWith encOpts filtered)



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
        , slLocalTimeSt       = floorMinute (slLocalTimeSt  a )
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


maybeQualStat :: (a -> Text)
              -> (a -> (Spaced (Maybe b)))
              -> (a -> (Spaced (Maybe b)))
              -> (a -> (Spaced (Maybe b)))
              -> a
              -> Maybe (Stat b)
maybeQualStat meanQf meanF maxF minF a =
    case qFilter meanQf meanF a of
        Just _  -> maybeStat meanF maxF minF a
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
