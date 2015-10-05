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

import Control.Lens                         (Lens') 
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Csv.Streaming                   (Records)
import Data.List                            (groupBy, foldl1')
import Data.Text                            (Text, unpack)
import Data.Time.LocalTime                  (LocalTime)
import System.Directory                     (doesFileExist)
import System.Environment                   (getArgs)
import System.FilePath.Find                 (find, always, (~~?), fileName)

import Tmy.OneMinSolar.Functions
import Tmy.OneMinSolar.Types
import Tmy.OneMinSolar.FillInterp
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
        -- fill in missing data (and check)
        awInfilled = awProcess fillInterpAndCheck awStats
        slInfilled = slProcess fillInterpAndCheck slStats
        -- group into hours
        awStatGroups = groupBy (hourGrouper awLTimeSt) awInfilled
        slStatGroups = groupBy (hourGrouper slLTimeSt) slInfilled
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


mkAwStats :: Text -> LocalTime -> AwStats
mkAwStats stNum lt = AwStats stNum (LTime lt)
                        Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing


mkSlStats :: Text -> LocalTime -> SlStats
mkSlStats stNum lt = SlStats stNum (LTime lt)
                        Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
