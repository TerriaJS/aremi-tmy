{-# LANGUAGE OverloadedStrings #-}


-- TODO:
-- compare Local time in aw and sl records and ensure we have correct ones lined up
-- either:
--   think about quality columns
--   start averaging values


module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Csv.Streaming                   (Records(Cons, Nil))
import Data.Either                          (partitionEithers)
import Data.Text                            (unpack)
import Data.Time.LocalTime                  (localTimeOfDay, todMin)
import System.Directory                     (doesFileExist)
import System.Environment                   (getArgs)
import System.FilePath.Find                 (find, always, (~~?), fileName)

import Tmy.OneMinSolarTypes
import Tmy.Csv


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
        combined = combineAwSl awRecs slRecs
        (lefts, rights) = partitionEithers combined
        filtered = filter zeroMinutes rights
    mapM_ putStrLn lefts
    BL.appendFile fn (encodeDefaultOrderedByNameWith encOpts filtered)


zeroMinutes :: CombinedAwSlObs -> Bool
zeroMinutes c = (todMin . localTimeOfDay . awLocalStdTime . awRecord) c == 00


combineAwSl :: Records AutoWeatherObs -> Records SolarRadiationObs -> [Either String CombinedAwSlObs]
combineAwSl (Cons a rs)     (Cons b rs2)     = do comb CombinedAwSlObs a b : combineAwSl rs rs2
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

