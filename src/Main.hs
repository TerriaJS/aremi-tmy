{-# LANGUAGE OverloadedStrings #-}


-- TODO:
-- filter out only 00 mins
-- either:
--   think about quality columns
--   start averaging values


module Main where

import Control.Monad                        (forM_)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Csv.Streaming                   (Records(Cons, Nil))
import Data.Text                            (unpack)
import Data.Time.LocalTime                  (localTimeOfDay, todMin)
import System.Directory                     (doesFileExist)
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
    --mapM_ putStrLn awFiles
    --mapM_ putStrLn slFiles

    mapM_ (processCsvPair newCsv) (zip awFiles slFiles)


processCsvPair :: FilePath -> (FilePath, FilePath) -> IO ()
processCsvPair fn t@(aw, sl) = do
    putStrLn ("Processing " ++ show t)

    -- typed for clarity
    awRecs <- readIndexedCsv aw :: IO (Records AutoWeatherObs)
    slRecs <- readCsv sl :: IO (Records SolarRadiationObs)

    --mapRecords_ print awRecs
    --mapRecords_ print slRecs

    fnExists <- doesFileExist fn
    let encOpts = defaultEncodeOptions {encIncludeHeader = not fnExists}
        combined = (combineAwSl awRecs slRecs)
        -- TODO: re-enable when we're happier
        -- filtered = filter zeroMinutes combined
    BL.appendFile fn (encodeDefaultOrderedByNameWith encOpts combined)


zeroMinutes :: CombinedAwSlObs -> Bool
zeroMinutes c = (todMin . localTimeOfDay . awLocalStdTime . awRecord) c == 00


combineAwSl :: Records AutoWeatherObs -> Records SolarRadiationObs -> [CombinedAwSlObs]
combineAwSl (Cons (Right a) rs) (Cons (Right b) rs2) = do
    CombinedAwSlObs a b : combineAwSl rs rs2
combineAwSl a b = do
    -- case a of
    -- case b of
    -- TODO: what to do when something has failed
    []


awPref = "aw_"
slPref = "sl_"
globSuff = "*.txt"
