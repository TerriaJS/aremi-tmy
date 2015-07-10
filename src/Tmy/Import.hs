{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tmy.Import where

import Control.Applicative                  ((<$>), (<*>))
import Control.Monad                        (mplus)
import qualified Data.ByteString      as B  (dropWhile)
import qualified Data.ByteString.Lazy as BL (ByteString, readFile, empty)
import Data.Char                            (isSpace)
import Data.Csv                      hiding (decodeByName, decode)
import Data.Csv.Streaming                   (Records(Cons, Nil), decodeByName, decode)
import Data.HashMap.Strict                  (union)
import qualified Data.Vector as V           (length)
import Data.Text                            (Text, strip)
import Data.Time.Calendar                   (fromGregorianValid)
import Data.Time.Format                     (formatTime)
import Data.Time.LocalTime                  (LocalTime(LocalTime), makeTimeOfDayValid)
import GHC.Generics                         (Generic)
import System.Locale                        (iso8601DateFormat, defaultTimeLocale)


--data Stat a = Stat {val,min,max,stdDev :: a}


newtype Trimmed = Trimmed {unTrimmed :: Text} deriving (Show, Eq, Ord, ToField)

instance FromField Trimmed where
    parseField bs = Trimmed . strip <$> parseField bs


newtype Spaced a = Spaced {unSpaced :: a} deriving (Show, Eq, Ord, ToField)

instance FromField a => FromField (Spaced a) where
    parseField bs = Spaced <$> parseField (B.dropWhile (== 32) bs)


instance ToField LocalTime where
    toField lt = toField (formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) lt)


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
    --, awYearLocal           :: !Int    -- Year Month Day Hours Minutes in YYYY
    --, awMMLocal             :: !Int    -- MM
    --, awDDLocal             :: !Int    -- DD
    --, awHH24Local           :: !Int    -- HH24
    --, awMILocal             :: !Int    -- MI format in Local time
    , awLocalTime           :: !LocalTime
    --, awYearLocalStd        :: !Int    -- Year Month Day Hours Minutes in YYYY
    --, awMMLocalStd          :: !Int    -- MM
    --, awDDLocalStd          :: !Int    -- DD
    --, awHH24LocalStd        :: !Int    -- HH24
    --, awMILocalStd          :: !Int    -- MI format in Local standard time
    , awLocalStdTime           :: !LocalTime
    --, awYearUtc             :: !Int    -- Year Month Day Hours Minutes in YYYY
    --, awMMUtc               :: !Int    -- MM
    --, awDDUtc               :: !Int    -- DD
    --, awHH24Utc             :: !Int    -- HH24
    --, awMIUtc               :: !Int    -- MI format in Universal coordinated time
    , awUtcTime           :: !LocalTime
    , awPrecipSinceLast     :: !(Spaced (Maybe Double)) -- Precipitation since last (AWS) observation in mm
    , awPrecipQual          :: !Text   -- Quality of precipitation since last (AWS) observation value
    , awAirTemp             :: !(Spaced (Maybe Double)) -- Air Temperature in degrees Celsius
    , awAirTempQual         :: !Text   -- Quality of air temperature
    , awAirTempMax          :: !(Spaced (Maybe Double)) -- Air temperature (1-minute maximum) in degrees Celsius
    , awAirTempMaxQual      :: !Text   -- Quality of air temperature (1-minute maximum)
    , awAirTempMin          :: !(Spaced (Maybe Double)) -- Air temperature (1-minute minimum) in degrees Celsius
    , awAirTempMinQual      :: !Text   -- Quality of air temperature (1-minute minimum)
    , awWetBulbTemp         :: !(Spaced (Maybe Double)) -- Wet bulb temperature in degrees Celsius
    , awWetBulbTempQual     :: !Text   -- Quality of Wet bulb temperature
    , awWetBulbTempMax      :: !(Spaced (Maybe Double)) -- Wet bulb temperature (1 minute maximum) in degrees Celsius
    , awWetBulbTempMaxQual  :: !Text   -- Quality of wet bulb temperature (1 minute maximum)
    , awWetBulbTempMin      :: !(Spaced (Maybe Double)) -- Wet bulb temperature (1 minute minimum) in degrees Celsius
    , awWetBulbTempMinQual  :: !Text   -- Quality of wet bulb temperature (1 minute minimum)
    , awDewPointTemp        :: !(Spaced (Maybe Double)) -- Dew point temperature in degrees Celsius
    , awDewPointTempQual    :: !Text   -- Quality of dew point temperature
    , awDewPointTempMax     :: !(Spaced (Maybe Double)) -- Dew point temperature (1-minute maximum) in degrees Celsius
    , awDewPointTempMaxQual :: !Text   -- Quality of Dew point Temperature (1-minute maximum)
    , awDewPointTempMin     :: !(Spaced (Maybe Double)) -- Dew point temperature (1 minute minimum) in degrees Celsius
    , awDewPointTempMinQual :: !Text   -- Quality of Dew point Temperature (1 minute minimum)
    , awRelHumid            :: !(Spaced (Maybe Int))    -- Relative humidity in percentage %
    , awRelHumidQual        :: !Text   -- Quality of relative humidity
    , awRelHumidMax         :: !(Spaced (Maybe Int))    -- Relative humidity (1 minute maximum) in percentage %
    , awRelHumidMaxQual     :: !Text   -- Quality of relative humidity (1 minute maximum)
    , awRelHumidMin         :: !(Spaced (Maybe Int))    -- Relative humidity (1 minute minimum) in percentage %
    , awRelHumidMinQual     :: !Text   -- Quality of Relative humidity (1 minute minimum)
    , awWindSpeed           :: !(Spaced (Maybe Int))    -- Wind (1 minute) speed in km/h
    , awWindSpeedQual       :: !Text   -- Wind (1 minute) speed quality
    , awWindSpeedMin        :: !(Spaced (Maybe Int))    -- Minimum wind speed (over 1 minute) in km/h
    , awWindSpeedMinQual    :: !Text   -- Minimum wind speed (over 1 minute) quality
    , awWindDir             :: !(Spaced (Maybe Int))    -- Wind (1 minute) direction in degrees true
    , awWindDirQual         :: !Text   -- Wind (1 minute) direction quality
    , awWindStdDev          :: !(Spaced (Maybe Int))    -- Standard deviation of wind (1 minute)
    , awWindStdDevQual      :: !Text   -- Standard deviation of wind (1 minute) direction quality
    , awWindGustMax         :: !(Spaced (Maybe Int))    -- Maximum wind gust (over 1 minute) in km/h
    , awWindGustMaxQual     :: !Text   -- Maximum wind gust (over 1 minute) quality
    , awVisibility          :: !(Spaced (Maybe Double)) -- Visibility (automatic - one minute data) in km
    , awVisibilityQual      :: !Text   -- Quality of visibility (automatic - one minute data)
    , awMslPress            :: !(Spaced (Maybe Double)) -- Mean sea level pressure in hPa
    , awMslPressQual        :: !Text   -- Quality of mean sea level pressure
    , awStationLvlPress     :: !(Spaced (Maybe Double)) -- Station level pressure in hPa
    , awStationLvlPressQual :: !Text   -- Quality of station level pressure
    , awQnhPress            :: !(Spaced (Maybe Double)) -- QNH pressure in hPa
    , awQnhPressQual        :: !Text   -- Quality of QNH pressure
    } deriving (Show, Eq, Ord, Generic)

instance FromRecord AutoWeatherObs where
    parseRecord v
        | V.length v == 62 =
            AutoWeatherObs  -- ignoring col 0: aw
                            -- TODO: can we do something better here?
                            --   at least join 5 per line when we know we're keeping all of them
                            <$> v .! 1         -- awStationNum
                            -- 2: awYearLocal, 3: awMMLocal, 4: awDDLocal, 5: awHH24Local, 6: awMILocal
                            <*> fieldsToLocalTime 2 v
                            -- 7: awYearLocalStd, 8: awMMLocalStd, 9: awDDLocalStd, 10: awHH24LocalStd, 11: awMILocalStd
                            <*> fieldsToLocalTime 7 v
                            -- 12: awYearUtc, 13: awMMUtc, 14: awDDUtc, 15: awHH24Utc, 16: awMIUtc
                            <*> fieldsToLocalTime 12 v
                            <*> v .! 17        -- awPrecipSinceLast
                            <*> v .! 18        -- awPrecipQual
                            <*> v .! 19        -- awAirTemp
                            <*> v .! 20        -- awAirTempQual
                            <*> v .! 21        -- awAirTempMax
                            <*> v .! 22        -- awAirTempMaxQual
                            <*> v .! 23        -- awAirTempMin
                            <*> v .! 24        -- awAirTempMinQual
                            <*> v .! 25        -- awWetBulbTemp
                            <*> v .! 26        -- awWetBulbTempQual
                            <*> v .! 27        -- awWetBulbTempMax
                            <*> v .! 28        -- awWetBulbTempMaxQual
                            <*> v .! 29        -- awWetBulbTempMin
                            <*> v .! 30        -- awWetBulbTempMinQual
                            <*> v .! 31        -- awDewPointTemp
                            <*> v .! 32        -- awDewPointTempQual
                            <*> v .! 33        -- awDewPointTempMax
                            <*> v .! 34        -- awDewPointTempMaxQual
                            <*> v .! 35        -- awDewPointTempMin
                            <*> v .! 36        -- awDewPointTempMinQual
                            <*> v .! 37        -- awRelHumid
                            <*> v .! 38        -- awRelHumidQual
                            <*> v .! 39        -- awRelHumidMax
                            <*> v .! 40        -- awRelHumidMaxQual
                            <*> v .! 41        -- awRelHumidMin
                            <*> v .! 42        -- awRelHumidMinQual
                            <*> v .! 43        -- awWindSpeed
                            <*> v .! 44        -- awWindSpeedQual
                            <*> v .! 45        -- awWindSpeedMin
                            <*> v .! 46        -- awWindSpeedMinQual
                            <*> v .! 47        -- awWindDir
                            <*> v .! 48        -- awWindDirQual
                            <*> v .! 49        -- awWindStdDev
                            <*> v .! 50        -- awWindStdDevQual
                            <*> v .! 51        -- awWindGustMax
                            <*> v .! 52        -- awWindGustMaxQual
                            <*> v .! 53        -- awVisibility
                            <*> v .! 54        -- awVisibilityQual
                            <*> v .! 55        -- awMslPress
                            <*> v .! 56        -- awMslPressQual
                            <*> v .! 57        -- awStationLvlPress
                            <*> v .! 58        -- awStationLvlPressQual
                            <*> v .! 59        -- awQnhPress
                            <*> v .! 60        -- awQnhPressQual
                            -- ignoring col 61: #
        | otherwise = fail ("CSV expected to have 62 columns, actual: " ++ show (V.length v) ++ ", row: " ++ show v)

instance ToNamedRecord AutoWeatherObs
instance DefaultOrdered AutoWeatherObs


data SolarRadiationObs = SolarRadiationObs
    { slStationNum          :: !Trimmed                 -- Station Number
    --, slYearLocal           :: !Int                     -- Year Month Day Hours Minutes in YYYY
    --, slMMLocal             :: !Int                     -- MM
    --, slDDLocal             :: !Int                     -- DD
    --, slHH24Local           :: !Int                     -- HH24
    --, slMILocal             :: !Int                     -- MI format in Local time
    , slLocalTime           :: !LocalTime
    , slGhiMean             :: !(Spaced (Maybe Double)) -- Mean global irradiance (over 1 minute) in W/sq m
    , slGhiMin              :: !(Spaced (Maybe Double)) -- Minimum 1 second global irradiance (over 1 minute) in W/sq m
    , slGhiMax              :: !(Spaced (Maybe Double)) -- Maximum 1 second global irradiance (over 1 minute) in W/sq m
    , slGhiStdDev           :: !(Spaced (Maybe Double)) -- Standard deviation of global irradiance (over 1 minute) in W/sq m
    , slGhiMeanUncertainty  :: !(Spaced (Maybe Double)) -- Uncertainty in mean global irradiance (over 1 minute) in W/sq m
    , slDni                 :: !(Spaced (Maybe Double)) -- Mean direct irradiance (over 1 minute) in W/sq m
    , slDniMin              :: !(Spaced (Maybe Double)) -- Minimum 1 second direct irradiance (over 1 minute) in W/sq m
    , slDniMax              :: !(Spaced (Maybe Double)) -- Maximum 1 second direct irradiance (over 1 minute) in W/sq m
    , slDniStdDev           :: !(Spaced (Maybe Double)) -- Standard deviation of direct irradiance (over 1 minute) in W/sq m
    , slDniMeanUncertainty  :: !(Spaced (Maybe Double)) -- Uncertainty in mean direct irradiance (over 1 minute) in W/sq m
    , slDiffMean            :: !(Spaced (Maybe Double)) -- Mean diffuse irradiance (over 1 minute) in W/sq m
    , slDiffMin             :: !(Spaced (Maybe Double)) -- Minimum 1 second diffuse irradiance (over 1 minute) in W/sq m
    , slDiffMax             :: !(Spaced (Maybe Double)) -- Maximum 1 second diffuse irradiance (over 1 minute) in W/sq m
    , slDiffStdDev          :: !(Spaced (Maybe Double)) -- Standard deviation of diffuse irradiance (over 1 minute) in W/sq m
    , slDiffMeanUncertainty :: !(Spaced (Maybe Double)) -- Uncertainty in mean diffuse irradiance (over 1 minute) in W/sq m
    , slTerrMean            :: !(Spaced (Maybe Double)) -- Mean terrestrial irradiance (over 1 minute) in W/sq m
    , slTerrMin             :: !(Spaced (Maybe Double)) -- Minimum 1 second terrestrial irradiance (over 1 minute) in W/sq m
    , slTerrMax             :: !(Spaced (Maybe Double)) -- Maximum 1 second terrestrial irradiance (over 1 minute) in W/sq m
    , slTerrStdDev          :: !(Spaced (Maybe Double)) -- Standard deviation of terrestrial irradiance (over 1 minute) in W/sq m
    , slTerrMeanUncertainty :: !(Spaced (Maybe Double)) -- Uncertainty in mean terrestrial irradiance (over 1 minute) in W/sq m
    , slDhiMean             :: !(Spaced (Maybe Double)) -- Mean direct horizontal irradiance (over 1 minute) in W/sq m
    , slDhiMin              :: !(Spaced (Maybe Double)) -- Minimum 1 second direct horizontal irradiance (over 1 minute) in W/sq m
    , slDhiMax              :: !(Spaced (Maybe Double)) -- Maximum 1 second direct horizontal irradiance (over 1 minute) in W/sq m
    , slDhiStdDev           :: !(Spaced (Maybe Double)) -- Standard deviation of direct horizontal irradiance (over 1 minute) in W/sq m
    , slDhiMeanUncertainy   :: !(Spaced (Maybe Double)) -- Uncertainty in mean direct horizontal irradiance (over 1 minute) in W/sq m
    , slSunshineSecs96      :: !(Spaced (Maybe Int))                     -- Sunshine-seconds-96 (duration of DNI exceeding 96 W/sq m over 1 minute) in seconds
    , slSunshineSecs120     :: !(Spaced (Maybe Int))                     -- Sunshine-seconds-120 (duration of DNI exceeding 120 W/sq m over 1 minute) in seconds
    , slSunshineSecs144     :: !(Spaced (Maybe Int))                     -- Sunshine-seconds-144 (duration of DNI exceeding 144 W/sq m over 1 minute) in seconds
    , slZenith              :: !(Spaced (Maybe Double))                  -- Zenith distance in degrees
    } deriving (Show, Eq, Ord, Generic)

instance FromNamedRecord SolarRadiationObs where
    parseNamedRecord r =
        SolarRadiationObs <$> r .: "Station Number"
                            <*> colsToLocalTime (r .: "Year Month Day Hours Minutes in YYYY")
                                                (r .: "MM")
                                                (r .: "DD")
                                                (r .: "HH24")
                                                (r .: "MI format in Local time")
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

instance ToNamedRecord SolarRadiationObs
instance DefaultOrdered SolarRadiationObs


data CombinedAwSlObs = CombinedAwSlObs
    { awRecord :: AutoWeatherObs
    , slRecord :: SolarRadiationObs
    } deriving (Show, Eq, Ord, Generic)

instance ToNamedRecord CombinedAwSlObs where
    toNamedRecord (CombinedAwSlObs aw sl) = union (toNamedRecord aw) (toNamedRecord sl)
instance DefaultOrdered CombinedAwSlObs where
    headerOrder _ =
        [ "awStationNum"
        --, "awYearLocal"
        --, "awMMLocal"
        --, "awDDLocal"
        --, "awHH24Local"
        --, "awMILocal"
        , "awLocalTime"
        --, "awYearLocalStd"
        --, "awMMLocalStd"
        --, "awDDLocalStd"
        --, "awHH24LocalStd"
        --, "awMILocalStd"
        , "awLocalStdTime"
        --, "awYearUtc"
        --, "awMMUtc"
        --, "awDDUtc"
        --, "awHH24Utc"
        --, "awMIUtc"
        , "awUtcTime"
        , "awPrecipSinceLast"
        --, "awPrecipQual"
        , "awAirTemp"
        --, "awAirTempQual"
        , "awAirTempMax"
        --, "awAirTempMaxQual"
        , "awAirTempMin"
        --, "awAirTempMinQual"
        , "awWetBulbTemp"
        --, "awWetBulbTempQual"
        , "awWetBulbTempMax"
        --, "awWetBulbTempMaxQual"
        , "awWetBulbTempMin"
        --, "awWetBulbTempMinQual"
        , "awDewPointTemp"
        --, "awDewPointTempQual"
        , "awDewPointTempMax"
        --, "awDewPointTempMaxQual"
        , "awDewPointTempMin"
        --, "awDewPointTempMinQual"
        , "awRelHumid"
        --, "awRelHumidQual"
        , "awRelHumidMax"
        --, "awRelHumidMaxQual"
        , "awRelHumidMin"
        --, "awRelHumidMinQual"
        , "awWindSpeed"
        --, "awWindSpeedQual"
        , "awWindSpeedMin"
        --, "awWindSpeedMinQual"
        , "awWindDir"
        --, "awWindDirQual"
        , "awWindStdDev"
        --, "awWindStdDevQual"
        , "awWindGustMax"
        --, "awWindGustMaxQual"
        , "awVisibility"
        --, "awVisibilityQual"
        , "awMslPress"
        --, "awMslPressQual"
        , "awStationLvlPress"
        , "awStationLvlPressQual"
        , "awQnhPress"
        , "awQnhPressQual"
        --, "slStationNum"
        --, "slYearLocal"
        --, "slMMLocal"
        --, "slDDLocal"
        --, "slHH24Local"
        --, "slMILocal"
        , "slGhiMean"
        , "slGhiMin"
        , "slGhiMax"
        , "slGhiStdDev"
        , "slGhiMeanUncertainty"
        , "slDni"
        , "slDniMin"
        , "slDniMax"
        , "slDniStdDev"
        , "slDniMeanUncertainty"
        , "slDiffMean"
        , "slDiffMin"
        , "slDiffMax"
        , "slDiffStdDev"
        , "slDiffMeanUncertainty"
        , "slTerrMean"
        , "slTerrMin"
        , "slTerrMax"
        , "slTerrStdDev"
        , "slTerrMeanUncertainty"
        , "slDhiMean"
        , "slDhiMin"
        , "slDhiMax"
        , "slDhiStdDev"
        , "slDhiMeanUncertainy"
        , "slSunshineSecs96"
        , "slSunshineSecs120"
        , "slSunshineSecs144"
        , "slZenith"
        ]


fieldsToLocalTime :: Int -> Record -> Parser LocalTime
fieldsToLocalTime i v = do
    mplus (colsToLocalTime (v .! i) (v .! (i+1)) (v .! (i+2)) (v .! (i+3)) (v .! (i+4)))
          (fail $ "Could not parse date starting at col " ++ show i)


colsToLocalTime :: Parser Integer -> Parser Int -> Parser Int -> Parser Int -> Parser Int -> Parser LocalTime
colsToLocalTime y m d h mn = do
    mlt <- maybeLocalTime <$> y <*> m <*> d <*> h <*> mn
    maybe (fail $ "Could not parse date") return mlt


maybeLocalTime :: Integer -> Int -> Int -> Int -> Int -> Maybe LocalTime
maybeLocalTime y m d h mn = LocalTime <$> (fromGregorianValid y m d)
                                      <*> (makeTimeOfDayValid h mn 0)


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


readIndexedCsv :: FromRecord a => FilePath -> IO (Records a)
readIndexedCsv fn = do
    bs <- BL.readFile fn
    return (decode HasHeader bs)


printFailure :: String -> String -> IO ()
printFailure pre err = putStrLn (pre ++ err)
