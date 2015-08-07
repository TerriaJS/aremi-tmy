{-# LANGUAGE OverloadedStrings #-}

-- TODO:
--   combine wind direction using vector math
--     save stats for wind speed and direction to generate wind rose?
--   Filling in missing data? This shold probably be done in the actual TMY algo.

module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Csv.Streaming                   (Records)
import Data.List                            (groupBy, foldl1')
import Data.Text                            (unpack)
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
        awRecsList = concatRecs awRecs
        slRecsList = concatRecs slRecs
        -- group and compute stats for aw and sl separately
        awGroups = groupBy (hourGrouper awLTime) awRecsList
        slGroups = groupBy (hourGrouper slLTime) slRecsList
        -- AutoWeatherObs to StatAutoWeatherObs, filter out poor quality
        awStatGroups = map (map awToStat) awGroups
        slStatGroups = map (map slToStat) slGroups
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




