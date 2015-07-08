{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad                        (forM_)
import Data.Csv                             (Header)
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
            -- read the CSV files
            sitesMeta <- mapM readCsv args :: IO [Records OneMinSolarSite]
            -- zip the list of records with the CSV filename for later
            let sitesAndFiles = zip args sitesMeta :: [(String, Records OneMinSolarSite)]
            -- partially apply the filename of the CSV file to the processing function
            mapM_ (\(fn,recs) -> mapRecords_ (processSingleSite fn) recs) sitesAndFiles

            -- DEBUG
            forM_ sitesAndFiles $ \(fn,recs) -> do
                putStrLn ("CSV file: " ++ fn)
                mapRecords_ print recs


processSingleSite :: FilePath -> OneMinSolarSite -> IO ()
processSingleSite fn s = do
    let csvDir = (reverse . dropWhile ('/' /=) . reverse) fn
        stationNum = unpack (bomStationNum s)
        awGlob = awPref ++ stationNum ++ "_*" ++ suff
        slGlob = slPref ++ stationNum ++ "_*" ++ suff

    --putStrLn ("fn:     " ++ fn)
    --putStrLn ("csvDir: " ++ csvDir)
    --files <- getDirectoryContents csvDir

    awFiles <- find always (fileName ~~? awGlob) csvDir
    slFiles <- find always (fileName ~~? slGlob) csvDir
    mapM_ putStrLn awFiles
    mapM_ putStrLn slFiles

    --awRecs <- readCsv (csvDir ++ awPref ++ bomStationNum s ++ )
    --Records OneMinSolarSiteAwData <- readCsv
    --Records OneMinSolarSiteSlData <- readCsv


    -- for f in {aw,sl}_stationNum_YYYY_MM.txt; do
        -- process date-parallel-by-month {aw,sl} records into averaged conjoined hourly dataset (fn type needs to change?)
        -- write
    return ()


awPref = "aw_"
slPref = "sl_"
suff   = ".txt"
