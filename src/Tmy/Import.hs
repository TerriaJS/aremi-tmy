{-# LANGUAGE OverloadedStrings #-}

module Tmy.Import where

import Control.Applicative                  ((<$>), (<*>))
import Data.Csv                             (FromNamedRecord, parseNamedRecord, (.:))
import Data.Text                            (Text, strip)


data OneMinSolarSite = OneMinSolarSite
    { bomStationNum    :: !Int      -- Bureau of Meteorology station number
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
