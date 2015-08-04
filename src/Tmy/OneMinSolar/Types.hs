{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Tmy.OneMinSolar.Types where

import Control.Applicative                  ((<$>), (<*>), (<|>))
import Data.ByteString                      (empty)
import Data.Csv
import Data.HashMap.Strict                  (unions)
import Data.Semigroup                       (Sum(..))
import Data.Text                            (Text)
import qualified Data.Vector as V           (length)
import GHC.Generics                         (Generic)

import Tmy.Common
import Tmy.Csv


data OneMinSolarSite = OneMinSolarSite
    { bomStationNum    :: !(Spaced Text) -- Bureau of Meteorology station number
    , rainDistrictCode :: !(Spaced Text) -- Rainfall district code
    , name             :: !(Spaced Text) -- Station name
    , closed           :: !(Spaced Text) -- Month/Year site closed (MM/YYYY)
    , lat              :: !Double        -- Latitute
    , lon              :: !Double        -- Longitude
    , latLotDerMethod  :: !(Spaced Text) -- Method by which lat/lon derived
    , state            :: !(Spaced Text) -- State
    , stationMsl       :: !Double        -- Height of station above mean sea level in metres
    , barometerMsl     :: !Double        -- Height of barometer above sea level in metres
    , wmoNum           :: !(Spaced Text) -- WMO index number
    } deriving (Show, Eq, Ord)

instance FromNamedRecord OneMinSolarSite where
    parseNamedRecord r =
        OneMinSolarSite
            <$> r .: "Bureau of Meteorology station number"
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


data AwSlCombined = AwSlCombined (Maybe AwStats) (Maybe SlStats) deriving (Show, Eq, Ord, Generic)

instance DefaultOrdered AwSlCombined where
    {-
    To generate the column names using ghci for easier copy and paste:
    > mapM_ print . sort . keys $  toNamedRecord (Nothing :: Maybe SlStats)
    > mapM_ print . sort . keys $  toNamedRecord (Nothing :: Maybe AwStats)
    -}
    headerOrder _ =
        [ "station"
        , "local time"
        , "local std time"
        , "utc time"

        , "ghi mean"
        , "ghi count"
        , "ghi max"
        , "ghi min"

        , "dni mean"
        , "dni count"
        , "dni max"
        , "dni min"

        , "dhi mean"
        , "dhi count"
        , "dhi max"
        , "dhi min"

        , "diffuse mean"
        , "diffuse count"
        , "diffuse max"
        , "diffuse min"

        , "terrestrial mean"
        , "terrestrial count"
        , "terrestrial max"
        , "terrestrial min"

        , "seconds dni exceeding 120 W/sq m"
        , "seconds dni exceeding 144 W/sq m"
        , "seconds dni exceeding 96 W/sq m"

        , "zenith mean"
        , "zenith count"

        , "air temp mean"
        , "air temp count"
        , "air temp max"
        , "air temp min"

        , "wet bulb mean"
        , "wet bulb count"
        , "wet bulb max"
        , "wet bulb min"

        , "dew point mean"
        , "dew point count"
        , "dew point max"
        , "dew point min"

        , "relative humidity mean"
        , "relative humidity count"
        , "relative humidity max"
        , "relative humidity min"

        , "wind speed mean"
        , "wind speed count"
        , "wind speed max"
        , "wind speed min"

        , "precipitation"

        , "msl pressure mean"
        , "msl pressure count"

        , "qnh pressure mean"
        , "qnh pressure count"

        , "station level pressure mean"
        , "station level pressure count"

        , "visibility mean"
        , "visibility count"
        ]

instance ToNamedRecord AwSlCombined where
    toNamedRecord (AwSlCombined Nothing Nothing) = error "We should never have a completely empty record."
    toNamedRecord (AwSlCombined aw sl) =
        unions
            [ namedRecord
                [ "station" .= ((awStationNumSt <$> aw) <|> (slStationNumSt <$> sl))
                , "local time" .= ((awLTimeSt <$> aw) <|> (slLTimeSt <$> sl))
                ]
            , toNamedRecord aw
            , toNamedRecord sl
            ]


data AwStats = AwStats
    { awStationNumSt      :: !Text
    , awLTimeSt           :: !LTime
    , awLocalStdTimeSt    :: !LTime
    , awUtcTimeSt         :: !LTime
    , awPrecipSinceLastSt :: !(Maybe (Sum Double1Dec))
    , awAirTempSt         :: !(Maybe (Stat Double1Dec))
    , awWetBulbTempSt     :: !(Maybe (Stat Double1Dec))
    , awDewPointTempSt    :: !(Maybe (Stat Double1Dec))
    , awRelHumidSt        :: !(Maybe (Stat Double1Dec))
    , awWindSpeedSt       :: !(Maybe (Stat Double1Dec))
    , awWindDirSt         :: !(Maybe Int)   -- TODO: this one needs special vector math
    , awVisibilitySt      :: !(Maybe (SumCount Double1Dec))
    , awMslPressSt        :: !(Maybe (SumCount Double1Dec))
    , awStationLvlPressSt :: !(Maybe (SumCount Double1Dec))
    , awQnhPressSt        :: !(Maybe (SumCount Double1Dec))
    } deriving (Show, Eq, Ord)

instance ToNamedRecord (Maybe AwStats) where
    toNamedRecord a =
        unions
            [ namedRecord
                [ "local std time" .= maybe empty (toField . awLocalStdTimeSt) a
                , "utc time"       .= maybe empty (toField . awUtcTimeSt)      a
                , "precipitation"  .= (fmap getSum . awPrecipSinceLastSt   =<< a)
                ]
            , statRecord "air temp"                   ( awAirTempSt         =<< a)
            , statRecord "wet bulb"                   ( awWetBulbTempSt     =<< a)
            , statRecord "dew point"                  ( awDewPointTempSt    =<< a)
            , statRecord "relative humidity"          ( awRelHumidSt        =<< a)
            , statRecord "wind speed"                 ( awWindSpeedSt       =<< a)
            -- , statRecord "wind direction"          ( awWindDirSt         =<< a)
            , sumCountRecord "visibility"             ( awVisibilitySt      =<< a)
            , sumCountRecord "msl pressure"           ( awMslPressSt        =<< a)
            , sumCountRecord "station level pressure" ( awStationLvlPressSt =<< a)
            , sumCountRecord "qnh pressure"           ( awQnhPressSt        =<< a)
            ]


data AutoWeatherObs = AutoWeatherObs
    { -- ignoring col aw
    awStationNum            :: !(Spaced Text)           -- Station Number
    -- , awYearLocal     :: !Int  -- Year Month Day Hours Minutes in YYYY
    -- , awMMLocal       :: !Int  -- MM
    -- , awDDLocal       :: !Int  -- DD
    -- , awHH24Local     :: !Int  -- HH24
    -- , awMILocal       :: !Int  -- MI format in Local time
    , awLTime               :: !LTime
    -- , awYearLocalStd  :: !Int  -- Year Month Day Hours Minutes in YYYY
    -- , awMMLocalStd    :: !Int  -- MM
    -- , awDDLocalStd    :: !Int  -- DD
    -- , awHH24LocalStd  :: !Int  -- HH24
    -- , awMILocalStd    :: !Int  -- MI format in Local standard time
    , awLocalStdTime        :: !LTime
    -- , awYearUtc       :: !Int  -- Year Month Day Hours Minutes in YYYY
    -- , awMMUtc         :: !Int  -- MM
    -- , awDDUtc         :: !Int  -- DD
    -- , awHH24Utc       :: !Int  -- HH24
    -- , awMIUtc         :: !Int  -- MI format in Universal coordinated time
    , awUtcTime             :: !LTime
    , awPrecipSinceLast     :: !(Spaced (Maybe Double1Dec)) -- Precipitation since last (AWS) observation in mm
    , awPrecipQual          :: !(Spaced Char)               -- Quality of precipitation since last (AWS) observation value
    , awAirTemp             :: !(Spaced (Maybe Double1Dec)) -- Air Temperature in degrees Celsius
    , awAirTempQual         :: !(Spaced Char)               -- Quality of air temperature
    , awAirTempMax          :: !(Spaced (Maybe Double1Dec)) -- Air temperature (1-minute maximum) in degrees Celsius
    , awAirTempMaxQual      :: !(Spaced Char)               -- Quality of air temperature (1-minute maximum)
    , awAirTempMin          :: !(Spaced (Maybe Double1Dec)) -- Air temperature (1-minute minimum) in degrees Celsius
    , awAirTempMinQual      :: !(Spaced Char)               -- Quality of air temperature (1-minute minimum)
    , awWetBulbTemp         :: !(Spaced (Maybe Double1Dec)) -- Wet bulb temperature in degrees Celsius
    , awWetBulbTempQual     :: !(Spaced Char)               -- Quality of Wet bulb temperature
    , awWetBulbTempMax      :: !(Spaced (Maybe Double1Dec)) -- Wet bulb temperature (1 minute maximum) in degrees Celsius
    , awWetBulbTempMaxQual  :: !(Spaced Char)               -- Quality of wet bulb temperature (1 minute maximum)
    , awWetBulbTempMin      :: !(Spaced (Maybe Double1Dec)) -- Wet bulb temperature (1 minute minimum) in degrees Celsius
    , awWetBulbTempMinQual  :: !(Spaced Char)               -- Quality of wet bulb temperature (1 minute minimum)
    , awDewPointTemp        :: !(Spaced (Maybe Double1Dec)) -- Dew point temperature in degrees Celsius
    , awDewPointTempQual    :: !(Spaced Char)               -- Quality of dew point temperature
    , awDewPointTempMax     :: !(Spaced (Maybe Double1Dec)) -- Dew point temperature (1-minute maximum) in degrees Celsius
    , awDewPointTempMaxQual :: !(Spaced Char)               -- Quality of Dew point Temperature (1-minute maximum)
    , awDewPointTempMin     :: !(Spaced (Maybe Double1Dec)) -- Dew point temperature (1 minute minimum) in degrees Celsius
    , awDewPointTempMinQual :: !(Spaced Char)               -- Quality of Dew point Temperature (1 minute minimum)
    , awRelHumid            :: !(Spaced (Maybe Double1Dec)) -- Relative humidity in percentage %
    , awRelHumidQual        :: !(Spaced Char)               -- Quality of relative humidity
    , awRelHumidMax         :: !(Spaced (Maybe Double1Dec)) -- Relative humidity (1 minute maximum) in percentage %
    , awRelHumidMaxQual     :: !(Spaced Char)               -- Quality of relative humidity (1 minute maximum)
    , awRelHumidMin         :: !(Spaced (Maybe Double1Dec)) -- Relative humidity (1 minute minimum) in percentage %
    , awRelHumidMinQual     :: !(Spaced Char)               -- Quality of Relative humidity (1 minute minimum)
    , awWindSpeed           :: !(Spaced (Maybe Double1Dec)) -- Wind (1 minute) speed in km/h
    , awWindSpeedQual       :: !(Spaced Char)               -- Wind (1 minute) speed quality
    , awWindSpeedMin        :: !(Spaced (Maybe Double1Dec)) -- Minimum wind speed (over 1 minute) in km/h
    , awWindSpeedMinQual    :: !(Spaced Char)               -- Minimum wind speed (over 1 minute) quality
    , awWindDir             :: !(Spaced (Maybe Int))        -- Wind (1 minute) direction in degrees true
    , awWindDirQual         :: !(Spaced Char)               -- Wind (1 minute) direction quality
    -- , awWindStdDev          :: !(Spaced (Maybe Int))        -- Standard deviation of wind (1 minute)
    -- , awWindStdDevQual      :: !(Spaced Char)               -- Standard deviation of wind (1 minute) direction quality
    , awWindGustMax         :: !(Spaced (Maybe Double1Dec)) -- Maximum wind gust (over 1 minute) in km/h
    , awWindGustMaxQual     :: !(Spaced Char)               -- Maximum wind gust (over 1 minute) quality
    , awVisibility          :: !(Spaced (Maybe Double1Dec)) -- Visibility (automatic - one minute data) in km
    , awVisibilityQual      :: !(Spaced Char)               -- Quality of visibility (automatic - one minute data)
    , awMslPress            :: !(Spaced (Maybe Double1Dec)) -- Mean sea level pressure in hPa
    , awMslPressQual        :: !(Spaced Char)               -- Quality of mean sea level pressure
    , awStationLvlPress     :: !(Spaced (Maybe Double1Dec)) -- Station level pressure in hPa
    , awStationLvlPressQual :: !(Spaced Char)               -- Quality of station level pressure
    , awQnhPress            :: !(Spaced (Maybe Double1Dec)) -- QNH pressure in hPa
    , awQnhPressQual        :: !(Spaced Char)               -- Quality of QNH pressure
    } deriving (Show, Eq, Ord, Generic)

instance FromRecord AutoWeatherObs where
    parseRecord v
        | V.length v == 62 =
            AutoWeatherObs
                -- ignoring col 0: aw
                <$> v .! 1         -- awStationNum
                -- 2: awYearLocal, 3: awMMLocal, 4: awDDLocal, 5: awHH24Local, 6: awMILocal
                <*> fieldsToLTime 2 v
                -- 7: awYearLocalStd, 8: awMMLocalStd, 9: awDDLocalStd, 10: awHH24LocalStd, 11: awMILocalStd
                <*> fieldsToLTime 7 v
                -- 12: awYearUtc, 13: awMMUtc, 14: awDDUtc, 15: awHH24Utc, 16: awMIUtc
                <*> fieldsToLTime 12 v
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
                -- <*> v .! 49        -- awWindStdDev
                -- <*> v .! 50        -- awWindStdDevQual
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


data SlStats = SlStats
    { slStationNumSt      :: !Text
    , slLTimeSt           :: !LTime
    , slGhiSt             :: !(Maybe (Stat Double1Dec))
    , slDniSt             :: !(Maybe (Stat Double1Dec))
    , slDiffSt            :: !(Maybe (Stat Double1Dec))
    , slTerrSt            :: !(Maybe (Stat Double1Dec))
    , slDhiSt             :: !(Maybe (Stat Double1Dec))
    , slSunshineSecs96St  :: !(Maybe (Sum Int))
    , slSunshineSecs120St :: !(Maybe (Sum Int))
    , slSunshineSecs144St :: !(Maybe (Sum Int))
    , slZenithSt          :: !(Maybe (SumCount Double1Dec))  -- TODO: is it correct to just average the zenith angle?
    } deriving (Show, Eq, Ord)

instance ToNamedRecord (Maybe SlStats) where
    toNamedRecord a =
        unions
            [ namedRecord
                [ "seconds dni exceeding 96 W/sq m"  .= (fmap getSum . slSunshineSecs96St  =<< a)
                , "seconds dni exceeding 120 W/sq m" .= (fmap getSum . slSunshineSecs120St =<< a)
                , "seconds dni exceeding 144 W/sq m" .= (fmap getSum . slSunshineSecs144St =<< a)
                ]
            , statRecord     "ghi"         (slGhiSt    =<< a)
            , statRecord     "dni"         (slDniSt    =<< a)
            , statRecord     "diffuse"     (slDiffSt   =<< a)
            , statRecord     "terrestrial" (slTerrSt   =<< a)
            , statRecord     "dhi"         (slDhiSt    =<< a)
            , sumCountRecord "zenith"      (slZenithSt =<< a)
            ]


data SolarRadiationObs = SolarRadiationObs
    { slStationNum          :: !(Spaced Text)           -- Station Number
    --, slYearLocal           :: !Int                   -- Year Month Day Hours Minutes in YYYY
    --, slMMLocal             :: !Int                   -- MM
    --, slDDLocal             :: !Int                   -- DD
    --, slHH24Local           :: !Int                   -- HH24
    --, slMILocal             :: !Int                   -- MI format in Local time
    , slLTime               :: !LTime
    , slGhiMean             :: !(Spaced (Maybe Double1Dec)) -- Mean global irradiance (over 1 minute) in W/sq m
    , slGhiMin              :: !(Spaced (Maybe Double1Dec)) -- Minimum 1 second global irradiance (over 1 minute) in W/sq m
    , slGhiMax              :: !(Spaced (Maybe Double1Dec)) -- Maximum 1 second global irradiance (over 1 minute) in W/sq m
    -- , slGhiStdDev           :: !(Spaced (Maybe Double1Dec)) -- Standard deviation of global irradiance (over 1 minute) in W/sq m
    -- , slGhiMeanUncertainty  :: !(Spaced (Maybe Double1Dec)) -- Uncertainty in mean global irradiance (over 1 minute) in W/sq m
    , slDniMean             :: !(Spaced (Maybe Double1Dec)) -- Mean direct irradiance (over 1 minute) in W/sq m
    , slDniMin              :: !(Spaced (Maybe Double1Dec)) -- Minimum 1 second direct irradiance (over 1 minute) in W/sq m
    , slDniMax              :: !(Spaced (Maybe Double1Dec)) -- Maximum 1 second direct irradiance (over 1 minute) in W/sq m
    -- , slDniStdDev           :: !(Spaced (Maybe Double1Dec)) -- Standard deviation of direct irradiance (over 1 minute) in W/sq m
    -- , slDniMeanUncertainty  :: !(Spaced (Maybe Double1Dec)) -- Uncertainty in mean direct irradiance (over 1 minute) in W/sq m
    , slDiffMean            :: !(Spaced (Maybe Double1Dec)) -- Mean diffuse irradiance (over 1 minute) in W/sq m
    , slDiffMin             :: !(Spaced (Maybe Double1Dec)) -- Minimum 1 second diffuse irradiance (over 1 minute) in W/sq m
    , slDiffMax             :: !(Spaced (Maybe Double1Dec)) -- Maximum 1 second diffuse irradiance (over 1 minute) in W/sq m
    -- , slDiffStdDev          :: !(Spaced (Maybe Double1Dec)) -- Standard deviation of diffuse irradiance (over 1 minute) in W/sq m
    -- , slDiffMeanUncertainty :: !(Spaced (Maybe Double1Dec)) -- Uncertainty in mean diffuse irradiance (over 1 minute) in W/sq m
    , slTerrMean            :: !(Spaced (Maybe Double1Dec)) -- Mean terrestrial irradiance (over 1 minute) in W/sq m
    , slTerrMin             :: !(Spaced (Maybe Double1Dec)) -- Minimum 1 second terrestrial irradiance (over 1 minute) in W/sq m
    , slTerrMax             :: !(Spaced (Maybe Double1Dec)) -- Maximum 1 second terrestrial irradiance (over 1 minute) in W/sq m
    -- , slTerrStdDev          :: !(Spaced (Maybe Double1Dec)) -- Standard deviation of terrestrial irradiance (over 1 minute) in W/sq m
    -- , slTerrMeanUncertainty :: !(Spaced (Maybe Double1Dec)) -- Uncertainty in mean terrestrial irradiance (over 1 minute) in W/sq m
    , slDhiMean             :: !(Spaced (Maybe Double1Dec)) -- Mean direct horizontal irradiance (over 1 minute) in W/sq m
    , slDhiMin              :: !(Spaced (Maybe Double1Dec)) -- Minimum 1 second direct horizontal irradiance (over 1 minute) in W/sq m
    , slDhiMax              :: !(Spaced (Maybe Double1Dec)) -- Maximum 1 second direct horizontal irradiance (over 1 minute) in W/sq m
    -- , slDhiStdDev           :: !(Spaced (Maybe Double1Dec)) -- Standard deviation of direct horizontal irradiance (over 1 minute) in W/sq m
    -- , slDhiMeanUncertainy   :: !(Spaced (Maybe Double1Dec)) -- Uncertainty in mean direct horizontal irradiance (over 1 minute) in W/sq m
    , slSunshineSecs96      :: !(Spaced (Maybe Int))    -- Sunshine-seconds-96 (duration of DNI exceeding 96 W/sq m over 1 minute) in seconds
    , slSunshineSecs120     :: !(Spaced (Maybe Int))    -- Sunshine-seconds-120 (duration of DNI exceeding 120 W/sq m over 1 minute) in seconds
    , slSunshineSecs144     :: !(Spaced (Maybe Int))    -- Sunshine-seconds-144 (duration of DNI exceeding 144 W/sq m over 1 minute) in seconds
    , slZenith              :: !(Spaced (Maybe Double1Dec)) -- Zenith distance in degrees
    } deriving (Show, Eq, Ord, Generic)

instance FromNamedRecord SolarRadiationObs where
    parseNamedRecord r =
        SolarRadiationObs
            <$> r .: "Station Number"
            <*> colsToLTime
                (r .: "Year Month Day Hours Minutes in YYYY")
                (r .: "MM")
                (r .: "DD")
                (r .: "HH24")
                (r .: "MI format in Local time")
            <*> r .: "Mean global irradiance (over 1 minute) in W/sq m"
            <*> r .: "Minimum 1 second global irradiance (over 1 minute) in W/sq m"
            <*> r .: "Maximum 1 second global irradiance (over 1 minute) in W/sq m"
            -- <*> r .: "Standard deviation of global irradiance (over 1 minute) in W/sq m"
            -- <*> r .: "Uncertainty in mean global irradiance (over 1 minute) in W/sq m"
            <*> r .: "Mean direct irradiance (over 1 minute) in W/sq m"
            <*> r .: "Minimum 1 second direct irradiance (over 1 minute) in W/sq m"
            <*> r .: "Maximum 1 second direct irradiance (over 1 minute) in W/sq m"
            -- <*> r .: "Standard deviation of direct irradiance (over 1 minute) in W/sq m"
            -- <*> r .: "Uncertainty in mean direct irradiance (over 1 minute) in W/sq m"
            <*> r .: "Mean diffuse irradiance (over 1 minute) in W/sq m"
            <*> r .: "Minimum 1 second diffuse irradiance (over 1 minute) in W/sq m"
            <*> r .: "Maximum 1 second diffuse irradiance (over 1 minute) in W/sq m"
            -- <*> r .: "Standard deviation of diffuse irradiance (over 1 minute) in W/sq m"
            -- <*> r .: "Uncertainty in mean diffuse irradiance (over 1 minute) in W/sq m"
            <*> r .: "Mean terrestrial irradiance (over 1 minute) in W/sq m"
            <*> r .: "Minimum 1 second terrestrial irradiance (over 1 minute) in W/sq m"
            <*> r .: "Maximum 1 second terrestrial irradiance (over 1 minute) in W/sq m"
            -- <*> r .: "Standard deviation of terrestrial irradiance (over 1 minute) in W/sq m"
            -- <*> r .: "Uncertainty in mean terrestrial irradiance (over 1 minute) in W/sq m"
            <*> r .: "Mean direct horizontal irradiance (over 1 minute) in W/sq m"
            <*> r .: "Minimum 1 second direct horizontal irradiance (over 1 minute) in W/sq m"
            <*> r .: "Maximum 1 second direct horizontal irradiance (over 1 minute) in W/sq m"
            -- <*> r .: "Standard deviation of direct horizontal irradiance (over 1 minute) in W/sq m"
            -- <*> r .: "Uncertainty in mean direct horizontal irradiance (over 1 minute) in W/sq m"
            <*> r .: "Sunshine-seconds-96 (duration of DNI exceeding 96 W/sq m over 1 minute) in seconds"
            <*> r .: "Sunshine-seconds-120 (duration of DNI exceeding 120 W/sq m over 1 minute) in seconds"
            <*> r .: "Sunshine-seconds-144 (duration of DNI exceeding 144 W/sq m over 1 minute) in seconds"
            <*> r .: "Zenith distance in degrees"

