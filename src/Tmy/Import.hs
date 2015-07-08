{-# LANGUAGE OverloadedStrings #-}

module Tmy.Import where

import Control.Applicative                  ((<$>), (<*>))
import qualified Data.ByteString.Lazy as BL (ByteString, readFile, empty)
import Data.Csv                             (FromNamedRecord, parseNamedRecord, (.:))
import Data.Csv.Streaming                   (Records(Cons, Nil), decodeByName)
import Data.Text                            (Text, strip)


data OneMinSolarSite = OneMinSolarSite
    { bomStationNum    :: !Text     -- Bureau of Meteorology station number
    , rainDistrictCode :: !Text     -- Rainfall district code
    , name             :: !Text     -- Station name
    , closed           :: !Text     -- Month/Year site closed (MM/YYYY)
    , lat              :: !Double   -- Latitute
    , lon              :: !Double   -- Longitude
    , latLotDerMethod  :: !Text     -- Method by which lat/lon derived
    , state            :: !Text     -- State
    , stationMsl       :: !Double   -- Height of station above mean sea level in metres
    , barometerMsl     :: !Double   -- Height of barometer above sea level in metres
    , wmoNum           :: !Text     -- WMO index number
    } deriving (Show, Eq, Ord)

instance FromNamedRecord OneMinSolarSite where
    parseNamedRecord r =
        OneMinSolarSite <$> r .: "Bureau of Meteorology station number"
                        <*> r .: "Rainfall district code"
                        <*> r .: "Station name"
                        <*> r .: "Month/Year site closed (MM/YYYY)"
                        <*> r .: "Latitute"
                        <*> r .: "Longitude"
                        <*> r .: "Method by which lat/lon derived"
                        <*> r .: "State"
                        <*> r .: "Height of station above mean sea level in metres"
                        <*> r .: "Height of barometer above sea level in metres"
                        <*> r .: "WMO index number"


data OneMinSolarSiteData = OneMinSolarSiteData
    { foo :: !Int
    , bar :: !Text
    } deriving (Show, Eq, Ord)

instance FromNamedRecord OneMinSolarSiteData where
    parseNamedRecord r =
        OneMinSolarSiteData <$> r .: "Foo"
                            <*> r .: "Bar"


mapRecords_ :: (a -> IO ()) -> Records a -> IO ()
mapRecords_ f (Cons eith rs) = do
    either (printFailure "Record failed: ") f eith
    mapRecords_ f rs
mapRecords_ _ (Nil err _) = do
    maybe (return ()) (printFailure "Failed to parse: ") err


readCsv :: FromNamedRecord a => String -> IO (Records a)
readCsv fn = do
    bs <- BL.readFile fn
    case decodeByName bs of
        Left err -> do
            putStrLn ("Failed to read file '" ++ fn ++ "': " ++ err)
            return (Nil Nothing BL.empty)
        Right (_, rs) -> return rs


printFailure :: String -> String -> IO ()
printFailure pre err = putStrLn (pre ++ err)
