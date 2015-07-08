{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad                        (forM_)
import Data.Csv                             (Header)
import Data.Csv.Streaming                   (Records(Cons, Nil))
import System.Environment                   (getArgs)

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
            --
            mapM_ (\(fn,recs) -> mapRecords_ (processSingleSite fn) recs) sitesAndFiles

            -- DEBUG
            forM_ sitesAndFiles $ \(fn,recs) -> do
                putStrLn ("CSV file: " ++ fn)
                mapRecords_ print recs


processSingleSite :: String -> OneMinSolarSite -> IO ()
processSingleSite fn s = do
    --let csvDir = (directory . fromText) fn

    --awRecs <- readCsv
    --Records OneMinSolarSiteAwData <- readCsv
    --Records OneMinSolarSiteSlData <- readCsv


    -- for f in {aw,sl}_stationNum_YYYY_MM.txt; do
        -- process date-parallel-by-month {aw,sl} records into averaged conjoined hourly dataset (fn type needs to change?)
        -- write
    return ()


awPref = "aw_"
slPref = "sl_"
suff   = ".txt"
