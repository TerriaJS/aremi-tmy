{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Tmy.Import where

import Control.Applicative                  ((<$>), (<*>))
import qualified Data.ByteString.Lazy as BL (ByteString, readFile, empty)
import Data.Csv                             (FromField, FromRecord, FromNamedRecord, parseField, parseRecord, parseNamedRecord, (.:), (.!))
import Data.Csv.Streaming                   (Records(Cons, Nil), decodeByName)
import qualified Data.Vector as V           (length)
import GHC.Generics                         (Generic)
import Data.Text                            (Text, strip)


--data Stat a = Stat {val,min,max,stdDev :: a}


newtype Trimmed = Trimmed {unTrimmed :: Text} deriving (Show, Eq, Ord)

instance FromField Trimmed where
    parseField bs = Trimmed . strip <$> parseField bs


data OneMinSolarSite = OneMinSolarSite
    { bomStationNum    :: !Trimmed -- Bureau of Meteorology station number
    , rainDistrictCode :: !Trimmed -- Rainfall district code
    , name             :: !Trimmed -- Station name
    , closed           :: !Trimmed -- Month/Year site closed (MM/YYYY)
    , lat              :: !Double  -- Latitute
    , lon              :: !Double  -- Longitude
    , latLotDerMethod  :: !Trimmed -- Method by which lat/lon derived
    , state            :: !Trimmed -- State
    , stationMsl       :: !Double  -- Height of station above mean sea level in metres
    , barometerMsl     :: !Double  -- Height of barometer above sea level in metres
    , wmoNum           :: !Trimmed -- WMO index number
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


data AutoWeatherObs = AutoWeatherObs
    { -- ignoring col aw
      awStationNum          :: !Trimmed  -- Station Number
    , awYearLocal           :: !Int    -- Year Month Day Hours Minutes in YYYY
    , awMMLocal             :: !Int    -- MM
    , awDDLocal             :: !Int    -- DD
    , awHH24Local           :: !Int    -- HH24
    , awMILocal             :: !Int    -- MI format in Local time
    , awYearLocalStd        :: !Int    -- Year Month Day Hours Minutes in YYYY
    , awMMLocalStd          :: !Int    -- MM
    , awDDLocalStd          :: !Int    -- DD
    , awHH24LocalStd        :: !Int    -- HH24
    , awMILocalStd          :: !Int    -- MI format in Local standard time
    , awYearUtc             :: !Int    -- Year Month Day Hours Minutes in YYYY
    , awMMUtc               :: !Int    -- MM
    , awDDUtc               :: !Int    -- DD
    , awHH24Utc             :: !Int    -- HH24
    , awMIUtc               :: !Int    -- MI format in Universal coordinated time
    , awPrecipSinceLast     :: !Double -- Precipitation since last (AWS) observation in mm
    , awPrecipQual          :: !Text   -- Quality of precipitation since last (AWS) observation value
    , awAirTemp             :: !Double -- Air Temperature in degrees Celsius
    , awAirTempQual         :: !Text   -- Quality of air temperature
    , awAirTempMax          :: !Double -- Air temperature (1-minute maximum) in degrees Celsius
    , awAirTempMaxQual      :: !Text   -- Quality of air temperature (1-minute maximum)
    , awAirTempMin          :: !Double -- Air temperature (1-minute minimum) in degrees Celsius
    , awAirTempMinQual      :: !Text   -- Quality of air temperature (1-minute minimum)
    , awWetBulbTemp         :: !Double -- Wet bulb temperature in degrees Celsius
    , awWetBulbTempQual     :: !Text   -- Quality of Wet bulb temperature
    , awWetBulbTempMax      :: !Double -- Wet bulb temperature (1 minute maximum) in degrees Celsius
    , awWetBulbTempMaxQual  :: !Text   -- Quality of wet bulb temperature (1 minute maximum)
    , awWetBulbTempMin      :: !Double -- Wet bulb temperature (1 minute minimum) in degrees Celsius
    , awWetBulbTempMinQual  :: !Text   -- Quality of wet bulb temperature (1 minute minimum)
    , awDewPointTemp        :: !Double -- Dew point temperature in degrees Celsius
    , awDewPointTempQual    :: !Text   -- Quality of dew point temperature
    , awDewPointTempMax     :: !Double -- Dew point temperature (1-minute maximum) in degrees Celsius
    , awDewPointTempMaxQual :: !Text   -- Quality of Dew point Temperature (1-minute maximum)
    , awDewPointTempMin     :: !Double -- Dew point temperature (1 minute minimum) in degrees Celsius
    , awDewPointTempMinQual :: !Text   -- Quality of Dew point Temperature (1 minute minimum)
    , awRelHumid            :: !Int    -- Relative humidity in percentage %
    , awRelHumidQual        :: !Text   -- Quality of relative humidity
    , awRelHumidMax         :: !Int    -- Relative humidity (1 minute maximum) in percentage %
    , awRelHumidMaxQual     :: !Text   -- Quality of relative humidity (1 minute maximum)
    , awRelHumidMin         :: !Int    -- Relative humidity (1 minute minimum) in percentage %
    , awRelHumidMinQual     :: !Text   -- Quality of Relative humidity (1 minute minimum)
    , awWindSpeed           :: !Int    -- Wind (1 minute) speed in km/h
    , awWindSpeedQual       :: !Text   -- Wind (1 minute) speed quality
    , awWindSpeedMin        :: !Int    -- Minimum wind speed (over 1 minute) in km/h
    , awWindSpeedMinQual    :: !Text   -- Minimum wind speed (over 1 minute) quality
    , awWindDir             :: !Int    -- Wind (1 minute) direction in degrees true
    , awWindDirQual         :: !Text   -- Wind (1 minute) direction quality
    , awWindStdDev          :: !Int    -- Standard deviation of wind (1 minute)
    , awWindStdDevQual      :: !Text   -- Standard deviation of wind (1 minute) direction quality
    , awWindGustMax         :: !Int    -- Maximum wind gust (over 1 minute) in km/h
    , awWindGustMaxQual     :: !Text   -- Maximum wind gust (over 1 minute) quality
    , awVisibility          :: !Double -- Visibility (automatic - one minute data) in km
    , awVisibilityQual      :: !Text   -- Quality of visibility (automatic - one minute data)
    , awMslPress            :: !Double -- Mean sea level pressure in hPa
    , awMslPressQual        :: !Text   -- Quality of mean sea level pressure
    , awStationLvlPress     :: !Double -- Station level pressure in hPa
    , awStationLvlPressQual :: !Text   -- Quality of station level pressure
    , awQnhPress            :: !Double -- QNH pressure in hPa
    , awQnhPressQual        :: !Text   -- Quality of QNH pressure
    } deriving (Show, Eq, Ord)

instance FromRecord AutoWeatherObs where
    parseRecord v
        | V.length v == 62 =
            AutoWeatherObs  -- ignoring col aw
                            <$> v .! 1
                            <*> v .! 2
                            <*> v .! 3
                            <*> v .! 4
                            <*> v .! 5
                            <*> v .! 6
                            <*> v .! 7
                            <*> v .! 8
                            <*> v .! 9
                            <*> v .! 10
                            <*> v .! 11
                            <*> v .! 12
                            <*> v .! 13
                            <*> v .! 14
                            <*> v .! 15
                            <*> v .! 16
                            <*> v .! 17
                            <*> v .! 18
                            <*> v .! 19
                            <*> v .! 20
                            <*> v .! 21
                            <*> v .! 22
                            <*> v .! 23
                            <*> v .! 24
                            <*> v .! 25
                            <*> v .! 26
                            <*> v .! 27
                            <*> v .! 28
                            <*> v .! 29
                            <*> v .! 30
                            <*> v .! 31
                            <*> v .! 32
                            <*> v .! 33
                            <*> v .! 34
                            <*> v .! 35
                            <*> v .! 36
                            <*> v .! 37
                            <*> v .! 38
                            <*> v .! 39
                            <*> v .! 40
                            <*> v .! 41
                            <*> v .! 42
                            <*> v .! 43
                            <*> v .! 44
                            <*> v .! 45
                            <*> v .! 46
                            <*> v .! 47
                            <*> v .! 48
                            <*> v .! 49
                            <*> v .! 50
                            <*> v .! 51
                            <*> v .! 52
                            <*> v .! 53
                            <*> v .! 54
                            <*> v .! 55
                            <*> v .! 56
                            <*> v .! 57
                            <*> v .! 58
                            <*> v .! 59
                            <*> v .! 60
                            -- ignoring col #
        | otherwise = fail ("CSV expected to have 62 columns, actual: " ++ show (V.length v) ++ ", row: " ++ show v)


data SolarRadiationObs = SolarRadiationObs
    { slStationNum          :: !Trimmed -- Station Number
    , slYearLocal           :: !Int     -- Year Month Day Hours Minutes in YYYY
    , slMMLocal             :: !Int     -- MM
    , slDDLocal             :: !Int     -- DD
    , slHH24Local           :: !Int     -- HH24
    , slMILocal             :: !Int     -- MI format in Local time
    , slGhiMean             :: !Text    -- Mean global irradiance (over 1 minute) in W/sq m
    , slGhiMin              :: !Text    -- Minimum 1 second global irradiance (over 1 minute) in W/sq m
    , slGhiMax              :: !Text    -- Maximum 1 second global irradiance (over 1 minute) in W/sq m
    , slGhiStdDev           :: !Text    -- Standard deviation of global irradiance (over 1 minute) in W/sq m
    , slGhiMeanUncertainty  :: !Text    -- Uncertainty in mean global irradiance (over 1 minute) in W/sq m
    , slDni                 :: !Text    -- Mean direct irradiance (over 1 minute) in W/sq m
    , slDniMin              :: !Text    -- Minimum 1 second direct irradiance (over 1 minute) in W/sq m
    , slDniMax              :: !Text    -- Maximum 1 second direct irradiance (over 1 minute) in W/sq m
    , slDniStdDev           :: !Text    -- Standard deviation of direct irradiance (over 1 minute) in W/sq m
    , slDniMeanUncertainty  :: !Text    -- Uncertainty in mean direct irradiance (over 1 minute) in W/sq m
    , slDiffMean            :: !Text    -- Mean diffuse irradiance (over 1 minute) in W/sq m
    , slDiffMin             :: !Text    -- Minimum 1 second diffuse irradiance (over 1 minute) in W/sq m
    , slDiffMax             :: !Text    -- Maximum 1 second diffuse irradiance (over 1 minute) in W/sq m
    , slDiffStdDev          :: !Text    -- Standard deviation of diffuse irradiance (over 1 minute) in W/sq m
    , slDiffMeanUncertainty :: !Text    -- Uncertainty in mean diffuse irradiance (over 1 minute) in W/sq m
    , slTerrMean            :: !Text    -- Mean terrestrial irradiance (over 1 minute) in W/sq m
    , slTerrMin             :: !Text    -- Minimum 1 second terrestrial irradiance (over 1 minute) in W/sq m
    , slTerrMax             :: !Text    -- Maximum 1 second terrestrial irradiance (over 1 minute) in W/sq m
    , slTerrStdDev          :: !Text    -- Standard deviation of terrestrial irradiance (over 1 minute) in W/sq m
    , slTerrMeanUncertainty :: !Text    -- Uncertainty in mean terrestrial irradiance (over 1 minute) in W/sq m
    , slDhiMean             :: !Text    -- Mean direct horizontal irradiance (over 1 minute) in W/sq m
    , slDhiMin              :: !Text    -- Minimum 1 second direct horizontal irradiance (over 1 minute) in W/sq m
    , slDhiMax              :: !Text    -- Maximum 1 second direct horizontal irradiance (over 1 minute) in W/sq m
    , slDhiStdDev           :: !Text    -- Standard deviation of direct horizontal irradiance (over 1 minute) in W/sq m
    , slDhiMeanUncertainy   :: !Text    -- Uncertainty in mean direct horizontal irradiance (over 1 minute) in W/sq m
    , slSunshineSecs96      :: !Int     -- Sunshine-seconds-96 (duration of DNI exceeding 96 W/sq m over 1 minute) in seconds
    , slSunshineSecs120     :: !Int     -- Sunshine-seconds-120 (duration of DNI exceeding 120 W/sq m over 1 minute) in seconds
    , slSunshineSecs144     :: !Int     -- Sunshine-seconds-144 (duration of DNI exceeding 144 W/sq m over 1 minute) in seconds
    , slZenith              :: !Double  -- Zenith distance in degrees
    } deriving (Show, Eq, Ord)

instance FromNamedRecord SolarRadiationObs where
    parseNamedRecord r =
        SolarRadiationObs <$> r .: "Station Number"
                            <*> r .: "Year Month Day Hours Minutes in YYYY"
                            <*> r .: "MM"
                            <*> r .: "DD"
                            <*> r .: "HH24"
                            <*> r .: "MI format in Local time"
                            <*> r .: "Mean global irradiance (over 1 minute) in W/sq m"
                            <*> r .: "Minimum 1 second global irradiance (over 1 minute) in W/sq m"
                            <*> r .: "Maximum 1 second global irradiance (over 1 minute) in W/sq m"
                            <*> r .: "Standard deviation of global irradiance (over 1 minute) in W/sq m"
                            <*> r .: "Uncertainty in mean global irradiance (over 1 minute) in W/sq m"
                            <*> r .: "Mean direct irradiance (over 1 minute) in W/sq m"
                            <*> r .: "Minimum 1 second direct irradiance (over 1 minute) in W/sq m"
                            <*> r .: "Maximum 1 second direct irradiance (over 1 minute) in W/sq m"
                            <*> r .: "Standard deviation of direct irradiance (over 1 minute) in W/sq m"
                            <*> r .: "Uncertainty in mean direct irradiance (over 1 minute) in W/sq m"
                            <*> r .: "Mean diffuse irradiance (over 1 minute) in W/sq m"
                            <*> r .: "Minimum 1 second diffuse irradiance (over 1 minute) in W/sq m"
                            <*> r .: "Maximum 1 second diffuse irradiance (over 1 minute) in W/sq m"
                            <*> r .: "Standard deviation of diffuse irradiance (over 1 minute) in W/sq m"
                            <*> r .: "Uncertainty in mean diffuse irradiance (over 1 minute) in W/sq m"
                            <*> r .: "Mean terrestrial irradiance (over 1 minute) in W/sq m"
                            <*> r .: "Minimum 1 second terrestrial irradiance (over 1 minute) in W/sq m"
                            <*> r .: "Maximum 1 second terrestrial irradiance (over 1 minute) in W/sq m"
                            <*> r .: "Standard deviation of terrestrial irradiance (over 1 minute) in W/sq m"
                            <*> r .: "Uncertainty in mean terrestrial irradiance (over 1 minute) in W/sq m"
                            <*> r .: "Mean direct horizontal irradiance (over 1 minute) in W/sq m"
                            <*> r .: "Minimum 1 second direct horizontal irradiance (over 1 minute) in W/sq m"
                            <*> r .: "Maximum 1 second direct horizontal irradiance (over 1 minute) in W/sq m"
                            <*> r .: "Standard deviation of direct horizontal irradiance (over 1 minute) in W/sq m"
                            <*> r .: "Uncertainty in mean direct horizontal irradiance (over 1 minute) in W/sq m"
                            <*> r .: "Sunshine-seconds-96 (duration of DNI exceeding 96 W/sq m over 1 minute) in seconds"
                            <*> r .: "Sunshine-seconds-120 (duration of DNI exceeding 120 W/sq m over 1 minute) in seconds"
                            <*> r .: "Sunshine-seconds-144 (duration of DNI exceeding 144 W/sq m over 1 minute) in seconds"
                            <*> r .: "Zenith distance in degrees"



mapRecords_ :: (a -> IO ()) -> Records a -> IO ()
mapRecords_ f (Cons eith rs) = do
    either (printFailure "Record failed: ") f eith
    mapRecords_ f rs
mapRecords_ _ (Nil err _) = do
    maybe (return ()) (printFailure "Failed to parse: ") err


readCsv :: FromNamedRecord a => FilePath -> IO (Records a)
readCsv fn = do
    bs <- BL.readFile fn
    case decodeByName bs of
        Left err -> do
            putStrLn ("Failed to read file '" ++ fn ++ "': " ++ err)
            return (Nil Nothing BL.empty)
        Right (_, rs) -> return rs


printFailure :: String -> String -> IO ()
printFailure pre err = putStrLn (pre ++ err)
