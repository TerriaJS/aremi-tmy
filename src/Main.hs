{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative                  ((<$>), (<*>))
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
            sitesMeta <- mapM readCsv args
            mapM_ (mapRecords_ processSingleSite) sitesMeta

            -- DEBUG
            mapM_ (mapRecords_ print) sitesMeta


processSingleSite :: OneMinSolarSite -> IO ()
processSingleSite s = do
    --Records OneMinSolarSiteAwData <- readCsv
    --Records OneMinSolarSiteSlData <- readCsv


    -- for f in {aw,sl}_stationNum_YYYY_MM.txt; do
        -- process date-parallel-by-month {aw,sl} records into averaged conjoined hourly dataset (fn type needs to change?)
        -- write
    return ()
