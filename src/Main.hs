{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Csv                             (Header)
import Data.Csv.Streaming                   (Records(Cons, Nil))
import System.Environment                   (getArgs)

import Tmy.Import


main :: IO ()
main = do
    args <- getArgs
    sitesMeta <- mapM readCsv args
    oneMinDatas <- mapM (sequence . processSites) sitesMeta

    -- DEBUG
    mapM_ print1minSolarSite sitesMeta


processSites :: Records OneMinSolarSite -> [IO (OneMinSolarSite, Records OneMinSolarSiteData)]
processSites (Cons eith rs) = do
    case eith of
        Left err -> do
            -- ehrmagerd what to do here?
            --printFailure err
            []
        Right r -> processSingleSite r : processSites rs
processSites (Nil err _) = do
    case err of
        Just message -> do
            -- ehrmagerd what to do here?
            --putStrLn ("Failed to parse: " ++ message)
            []
                         -- ehrmagerd what to do here?
        Nothing -> []    -- normal termination, no more sites


processSingleSite :: OneMinSolarSite -> IO (OneMinSolarSite, Records OneMinSolarSiteData)
processSingleSite s = do
    -- for f in {aw,sl}_stationNum_YYYY_MM.txt; do
        -- process date-parallel-by-month {aw,sl} records into averaged conjoined hourly dataset (fn type needs to change?)
    undefined


print1minSolarSite :: Records OneMinSolarSite -> IO ()
print1minSolarSite (Cons ei rs) = do
    either printFailure print ei
    print1minSolarSite rs
print1minSolarSite (Nil err _) = do
    case err of
        Nothing -> return ()
        Just message -> putStrLn ("Failed to parse: " ++ message)


printFailure :: String -> IO ()
printFailure err = putStrLn ("Record failed: " ++ err)
