{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad                        (forM_)
import qualified Data.ByteString.Lazy as BL (ByteString, readFile, empty)
import Data.Csv                             (FromRecord, ToNamedRecord)
import Data.Csv.Streaming                   (Records(Cons, Nil))
import Data.Text                            (unpack)
import System.Environment                   (getArgs)
import System.FilePath.Find                 (find, always, (~~?), fileName)

import Tmy.Import


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
        stationNum = (unpack . unTrimmed . bomStationNum) s
        awGlob = awPref ++ stationNum ++ globSuff
        slGlob = slPref ++ stationNum ++ globSuff
        newCsv = stationNum ++ "_averaged.csv"

    -- DEBUG
    --putStrLn ("fn:     " ++ fn)
    --putStrLn ("csvDir: " ++ csvDir)

    awFiles <- find always (fileName ~~? awGlob) csvDir
    slFiles <- find always (fileName ~~? slGlob) csvDir

    -- DEBUG
    mapM_ putStrLn awFiles
    --mapM_ putStrLn slFiles

    mapM_ (processCsvPair newCsv) (zip awFiles slFiles)

    -- for f in {aw,sl}_stationNum_YYYY_MM.txt; do
        -- process date-parallel-by-month {aw,sl} records into averaged conjoined hourly dataset (fn type needs to change?)
        -- write
    return ()


processCsvPair :: FilePath -> (FilePath, FilePath) -> IO ()
processCsvPair fn (aw, sl) = do
    -- typed for clarity
    awRecs <- readIndexedCsv aw :: IO (Records AutoWeatherObs)
    slRecs <- readCsv sl :: IO (Records SolarRadiationObs)

    --mapRecords_ print awRecs
    mapRecords_ print slRecs

    combineAwSl awRecs slRecs

    return ()


combineAwSl :: Records AutoWeatherObs -> Records SolarRadiationObs -> IO ()
combineAwSl (Cons (Right a) rs) (Cons (Right b) rs2) = do
    -- DEBUG
    print a
    print b

    combineAwSl rs rs2
combineAwSl a b = do
    -- case a of
    -- case b of
    putStrLn "TODO"


awPref = "aw_"
slPref = "sl_"
globSuff = "*.txt"
