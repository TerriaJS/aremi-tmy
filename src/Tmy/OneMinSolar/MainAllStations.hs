{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-- {-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ApplicativeDo     #-}


-- TODO:
--   save stats for wind speed and direction to generate wind rose?
--   create separate executables for the different stages so that we have something like:
--      * main executable that does everything
--      * load CSV and turn into AwSlCombined and save to CSV
--      * fill in missing values for <5 hour gaps (should this reuse previous code, or load from CSV?)
--      * fill in missing values for X hour gaps etc.


module Main where

import           Control.Lens
import qualified Data.ByteString.Lazy         as BL
-- import qualified Data.ByteString              as BS
import qualified Data.Vector                  as V

import qualified Data.HashMap.Strict          as M

import           Control.Monad
-- import           Data.Monoid ((<>))

import           Data.Csv
-- import Data.Csv.Streaming                   (Records)
-- import Data.List                            (groupBy, foldl1')
import           Data.Text                    (Text, unpack)
import           Data.Time.LocalTime          (LocalTime(..),todHour,localTimeToUTC,TimeZone)
-- import           Data.Vector (toList)

-- import Data.Time.Clock                      (UTCTime)
-- import           System.Directory             (doesFileExist)
import           System.Environment           (getArgs)
import           System.FilePath.Find         (always, fileName, find, (~~?))

import           Control.Monad.IO.Class
import           Pipes                        (Pipe)
import qualified Pipes                        as P
import qualified Pipes.Prelude                as P
import qualified Pipes.Csv                    as PC
import qualified Pipes.ByteString             as PBS
import           Pipes.Safe                   (runSafeT, SafeT, bracket)
import           Pipes.HTTP
import qualified Control.Foldl                as F

import           Tmy.Common
import           Tmy.Csv
-- import           Tmy.OneMinSolar.FillAdjacent
-- import           Tmy.OneMinSolar.FillInterp
-- import           Tmy.OneMinSolar.Functions
import           Tmy.OneMinSolar.TimeZones
import           Tmy.OneMinSolar.Types

-- import Data.Coerce

import           Control.Concurrent.Async     (mapConcurrently)
import System.IO (openFile, IOMode(..), hClose)

-- import Debug.Trace


main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "No files specified."
        else do
            -- read the CSV files, typed for clarity
            sitesMeta <- mapM (\name -> decode NoHeader <$> BL.readFile name) args
            -- zip the list of records with the CSV filename for later, typed for clarity
            let sitesAndFiles = zip args sitesMeta :: [(String, Either String (V.Vector StationMeta))]
            -- partially apply the filename of the CSV file to the processing function
            forM_ sitesAndFiles $ \(file,econtent) -> do
              case econtent of
                Left err -> putStrLn (file++": " ++ err)
                Right _ -> pure ()
            case sequenceA sitesMeta of
              Left err -> putStrLn err
              Right vs -> do
                let metadata = V.foldl (\mp sm -> M.insert (sm ^. smStationNum . to unSpaced) sm mp) M.empty $ V.concat vs
                mapM_ (\(file,Right sites) -> mapConcurrently (\site -> processSingleSite file site) sites) sitesAndFiles
                pure ()

            -- mapM_ (\(fn,recs) -> mapRecords_ (processSingleSite fn) recs) sitesAndFiles

            -- DEBUG
            {-
            forM_ sitesAndFiles $ \(fn,recs) -> do
                putStrLn ("CSV file: " ++ fn)
                mapRecords_ print recs
            -}


wsPrefix :: String
wsPrefix = "HM01X_Data_"
avPrefix :: String
avPrefix = "HD01D_Data_"
fileGlob :: String
fileGlob = "*.txt"

processSingleSite :: FilePath -> StationMeta -> IO ()
processSingleSite fn s = do
    let csvDir = (reverse . dropWhile ('/' /=) . reverse) fn
        stationNum = (unpack . unSpaced . _smStationNum) s
        state = unSpaced . _smState $ s
        bsGlob = wsPrefix ++ stationNum ++ fileGlob
        -- slGlob = avPrefix ++ stationNum ++ fileGlob
        newCsv = stationNum ++ "_averaged.csv"
        mtz    = getTZ (s ^. smStationNum . to unSpaced) (s ^. smState . to unSpaced)
    bsFiles <- find always (fileName ~~? bsGlob) csvDir
    -- avFiles <- find always (fileName ~~? slGlob) csvDir


    -- read all data into two long lists of records to avoid BoM month end/begin mismatch

    -- bsRecs <- concatRecs <$> mapM readIndexedCsv bsFiles :: IO [BoMStation]
    -- avRecs <- concatRecs <$> mapM readIndexedCsv avFiles :: IO [BoMAveStation]
    fnExists <- doesFileExist newCsv
    -- print bsRecs
    -- print avRecs
    hout <- openFile newCsv AppendMode
    unless fnExists $ BL.hPut hout (( <> "\n") . encode . pure . toList . headerOrder $ (undefined :: AwSlCombined))
    forM_ bsFiles $ \bsFile -> runSafeT $ do
      -- n <- P.length $ PC.decodeByName @AutoWeatherObs (readFileP bsFile)
      -- liftIO $ print (bsFile, n)
      pure ()

    pure()


readFileP :: FilePath -> P.Producer' PBS.ByteString (SafeT IO) ()
readFileP file = bracket
    (do h <- openFile file ReadMode
        putStrLn $ "{" ++ file ++ " open}"
        return h )
    (\h -> do
        hClose h
        putStrLn $ "{" ++ file ++ " closed}" )
    PBS.fromHandle

printErrors :: (MonadIO m) => Pipe (Either String a) a m b
printErrors = do
  e <- P.await
  either (liftIO . putStrLn) P.yield e
  printErrors


{-
    let encOpts = defaultEncodeOptions {encIncludeHeader = not fnExists}
        -- concatenate all records from all files as aw and sl file timestamps do not line up
        processor :: (Processor -> [a] -> [a]) -> (a -> a -> a) -> (a -> UTCTime) -> [a] -> [a]
        processor f agg getTime xs = let
          medFilled = f (composeProcessors fillAdjacent fillInterpAndCheck) xs
          -- medFilled   = f fillAdjacent shortFilled
          statGroups = groupBy (utcHourGrouper getTime) medFilled
          aggregated = map (foldl1' agg) statGroups
          in aggregated
        mapMaybe :: Show a => (a -> Maybe b) -> [a] -> [b]
        mapMaybe _ [] = []
        mapMaybe f (x:xs) = case f x of
            Nothing -> error $ "mapMaybe: " ++ show x
            Just b -> b : mapMaybe f xs
    -- if | not (null bsFiles) -> print (Prelude.length $ processor bsProcess const _bsLocalStdTime bsRecs)
    if | not (null bsFiles) -> do
          print ("Number of records:", stationNum,Prelude.length bsRecs)
          -- let tz = maybe (error "Could not find timezone for station: " ++ show s) id mtz

          print ("After processing:", stationNum,Prelude.length $
            processor bsProcess
                      const
                      (maybe (error "Record did not have UTCTime") id . _bsUTCTime)
                      $ mapMaybe (addUTC state bomStationStdTime (bsUTCTime . _Just)) bsRecs
                )

              -- slShortFilled = slProcess fillInterpAndCheck slStats
        -- turn the records into Stat recs, this filters out aw values by quality
        -- awStats = map awToStat awRecsList
        -- slStats = map slToStat slRecsList
        -- fill in missing data for small gaps via linear interpolation (and check)
        -- fill in missing data for medium gaps with adjacent days
        -- awMedFilled = awProcess fillAdjacent awShortFilled
        -- slMedFilled = slProcess fillAdjacent slShortFilled
        -- group into hours
        -- awStatGroups = groupBy (hourGrouper awLTimeSt) awMedFilled
        -- slStatGroups = groupBy (hourGrouper slLTimeSt) slMedFilled
        -- aggregate 1-minute records to hours
        -- awFolded = map (foldl1' awAggr) awStatGroups
        -- slFolded = map (foldl1' slAggr) slStatGroups
        -- !_ = traceShowId ((take 5) awFolded)
        -- !_ = traceShowId ((take 5) slFolded)
        -- combine 1-hour aw and sl records
        -- merged = mergeWith awLTimeSt slLTimeSt AwSlCombined awFolded slFolded
    if null (bsFiles++avFiles)
        then putStrLn ("No records found for station " ++ show stationNum)
        else do
            putStrLn ("Processing " ++ show newCsv)
            -- BL.appendFile newCsv (encodeDefaultOrderedByNameWith encOpts merged)
-}


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

ftDouble :: FieldType Double1Dec
ftDouble = FieldType
    { mkValue  = id
    , getValue = id
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


bsProcess :: Processor -> [BoMStation] -> [BoMStation]
bsProcess p xs =
    ( f (bsPrecip . coerced)        ftDouble
    . f (bsAirTemp . coerced)       ftDouble
    . f (bsWetBulbTemp . coerced)   ftDouble
    . f (bsDewPoint . coerced)      ftDouble
    . f (bsRelHumid . coerced)      ftDouble
    . f (bsVapourPres . coerced)    ftDouble
    . f (bsSatVapourPres . coerced) ftDouble
    . f (bsWindSpeed . coerced)     ftDouble
    . f (bsWindGustMax . coerced)   ftDouble
    . f (bsSeaLevPress . coerced)   ftDouble
    ) xs
    where
        f :: Show b => Lens' BoMStation (Maybe b) -> FieldType b -> [BoMStation] -> [BoMStation]
        f = p bsStatsP





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
    { lTime    = unLTime . awLTimeSt
    , stNum    = awStationNumSt
    , setLTime = \x t -> x { awLTimeSt = LTime t }
    , mkEmpty  = mkAwStats
    }


bsStatsP :: Processing BoMStation
bsStatsP = Processing
    { lTime    = unLTime . _bsLocalStdTime
    , stNum    = unSpaced . _bsStationNum
    , setLTime = \x t -> x { _bsLocalStdTime = LTime t }
    , mkEmpty  = mkBoMStation
    }

slStatsP :: Processing SlStats
slStatsP = Processing
    { lTime    = unLTime . slLTimeSt
    , stNum    = slStationNumSt
    , setLTime = \x t -> x { slLTimeSt = LTime t }
    , mkEmpty  = mkSlStats
    }


mkAwStats :: Text -> LocalTime -> AwStats
mkAwStats stNum lt = AwStats stNum (LTime lt)
                        Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

mkBoMStation :: Text -> LocalTime -> BoMStation
mkBoMStation stNum lt =
  BoMStation (Spaced stNum) (LTime lt) (LTime lt)
    (Spaced Nothing)
    (Spaced Nothing)
    (Spaced Nothing)
    (Spaced Nothing)
    (Spaced Nothing)
    (Spaced Nothing)
    (Spaced Nothing)
    (Spaced Nothing)
    (Spaced Nothing)
    (Spaced Nothing)
    (Spaced Nothing)
    (Spaced Nothing)
    (Spaced Nothing)
    (Spaced Nothing)
    (Spaced Nothing)
    (Spaced Nothing)
    (Spaced Nothing)
    (Spaced Nothing)
    (Spaced Nothing)
    (Spaced Nothing)
    (Spaced Nothing)
    (Spaced Nothing)
    (Spaced Nothing)
    Nothing


mkSlStats :: Text -> LocalTime -> SlStats
mkSlStats stNum lt = SlStats stNum (LTime lt)
                        Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
