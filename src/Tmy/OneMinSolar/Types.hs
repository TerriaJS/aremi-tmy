{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Tmy.OneMinSolar.Types where

import Control.Applicative                  ((<|>))
import Control.Lens                         (Lens', makeLenses, Getter, to, (^.))
-- import Data.ByteString                      (empty)
import Data.Csv
import Data.HashMap.Strict                  (unions)
import Data.Semigroup                       (Sum(..))
import Data.Text                            (Text)
import Data.Time.LocalTime                  (LocalTime)
import Data.Time.Clock                      (UTCTime)
import Data.Time.Format                     (parseTimeM, defaultTimeLocale, iso8601DateFormat)
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


data AwStats = AwStats
    { awStationNumSt       :: !Text
    , awLTimeSt            :: !LTime
    -- , awLocalStdTimeSt     :: !(Maybe LTime)
    -- , awUtcTimeSt          :: !(Maybe LTime)
    , _awPrecipSinceLastSt :: !(Maybe (Sum Double1Dec))
    , _awAirTempSt         :: !(Maybe (Stat Double1Dec))
    , _awWetBulbTempSt     :: !(Maybe (Stat Double1Dec))
    , _awDewPointTempSt    :: !(Maybe (Stat Double1Dec))
    , _awRelHumidSt        :: !(Maybe (Stat Double1Dec))
    , _awWindSpeedSt       :: !(Maybe (Stat Double1Dec))
    , awWindDirSt          :: !(Maybe Int)
    , _awVisibilitySt      :: !(Maybe (Mean Double1Dec))
    , _awMslPressSt        :: !(Maybe (Mean Double1Dec))
    , _awStationLvlPressSt :: !(Maybe (Mean Double1Dec))
    , _awQnhPressSt        :: !(Maybe (Mean Double1Dec))
    } deriving (Show, Eq, Ord)

makeLenses ''AwStats

instance ToNamedRecord (Maybe AwStats) where
    toNamedRecord a =
        unions
            [ namedRecord
                [ {-"local std time" .= maybe empty (toField . awLocalStdTimeSt)  a
                , "utc time"       .= maybe empty (toField . awUtcTimeSt)       a
                ,-} "precipitation"  .= (fmap getSum . _awPrecipSinceLastSt   =<< a)
                , "wind direction" .= (awWindDirSt                          =<< a)
                ]
            , statRecord "air temp"                   (_awAirTempSt         =<< a)
            , statRecord "wet bulb"                   (_awWetBulbTempSt     =<< a)
            , statRecord "dew point"                  (_awDewPointTempSt    =<< a)
            , statRecord "relative humidity"          (_awRelHumidSt        =<< a)
            , statRecord "wind speed"                 (_awWindSpeedSt       =<< a)
            , meanRecord "visibility"                 (_awVisibilitySt      =<< a)
            , meanRecord "msl pressure"               (_awMslPressSt        =<< a)
            , meanRecord "station level pressure"     (_awStationLvlPressSt =<< a)
            , meanRecord "qnh pressure"               (_awQnhPressSt        =<< a)
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
    -- , awLocalStdTime        :: !LTime
    -- , awYearUtc       :: !Int  -- Year Month Day Hours Minutes in YYYY
    -- , awMMUtc         :: !Int  -- MM
    -- , awDDUtc         :: !Int  -- DD
    -- , awHH24Utc       :: !Int  -- HH24
    -- , awMIUtc         :: !Int  -- MI format in Universal coordinated time
    -- , awUtcTime             :: !LTime
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
                <$> v .!! 1         -- awStationNum
                -- 2: awYearLocal, 3: awMMLocal, 4: awDDLocal, 5: awHH24Local, 6: awMILocal
                <*> fieldsToLTime 2 v
                -- 7: awYearLocalStd, 8: awMMLocalStd, 9: awDDLocalStd, 10: awHH24LocalStd, 11: awMILocalStd
                -- <*> fieldsToLTime 7 v
                -- 12: awYearUtc, 13: awMMUtc, 14: awDDUtc, 15: awHH24Utc, 16: awMIUtc
                -- <*> fieldsToLTime 12 v
                <*> v .!! 17        -- awPrecipSinceLast
                <*> v .!! 18        -- awPrecipQual
                <*> v .!! 19        -- awAirTemp
                <*> v .!! 20        -- awAirTempQual
                <*> v .!! 21        -- awAirTempMax
                <*> v .!! 22        -- awAirTempMaxQual
                <*> v .!! 23        -- awAirTempMin
                <*> v .!! 24        -- awAirTempMinQual
                <*> v .!! 25        -- awWetBulbTemp
                <*> v .!! 26        -- awWetBulbTempQual
                <*> v .!! 27        -- awWetBulbTempMax
                <*> v .!! 28        -- awWetBulbTempMaxQual
                <*> v .!! 29        -- awWetBulbTempMin
                <*> v .!! 30        -- awWetBulbTempMinQual
                <*> v .!! 31        -- awDewPointTemp
                <*> v .!! 32        -- awDewPointTempQual
                <*> v .!! 33        -- awDewPointTempMax
                <*> v .!! 34        -- awDewPointTempMaxQual
                <*> v .!! 35        -- awDewPointTempMin
                <*> v .!! 36        -- awDewPointTempMinQual
                <*> v .!! 37        -- awRelHumid
                <*> v .!! 38        -- awRelHumidQual
                <*> v .!! 39        -- awRelHumidMax
                <*> v .!! 40        -- awRelHumidMaxQual
                <*> v .!! 41        -- awRelHumidMin
                <*> v .!! 42        -- awRelHumidMinQual
                <*> v .!! 43        -- awWindSpeed
                <*> v .!! 44        -- awWindSpeedQual
                <*> v .!! 45        -- awWindSpeedMin
                <*> v .!! 46        -- awWindSpeedMinQual
                <*> v .!! 47        -- awWindDir
                <*> v .!! 48        -- awWindDirQual
                -- <*> v .!! 49        -- awWindStdDev
                -- <*> v .!! 50        -- awWindStdDevQual
                <*> v .!! 51        -- awWindGustMax
                <*> v .!! 52        -- awWindGustMaxQual
                <*> v .!! 53        -- awVisibility
                <*> v .!! 54        -- awVisibilityQual
                <*> v .!! 55        -- awMslPress
                <*> v .!! 56        -- awMslPressQual
                <*> v .!! 57        -- awStationLvlPress
                <*> v .!! 58        -- awStationLvlPressQual
                <*> v .!! 59        -- awQnhPress
                <*> v .!! 60        -- awQnhPressQual
                -- ignoring col 61: #
        | otherwise = fail ("CSV expected to have 62 columns, actual: " ++ show (V.length v) ++ ", row: " ++ show v)


data SlStats = SlStats
    { slStationNumSt       :: !Text
    , slLTimeSt            :: !LTime
    , _slGhiSt             :: !(Maybe (Stat Double1Dec))
    , _slDniSt             :: !(Maybe (Stat Double1Dec))
    , _slDiffSt            :: !(Maybe (Stat Double1Dec))
    , _slTerrSt            :: !(Maybe (Stat Double1Dec))
    , _slDhiSt             :: !(Maybe (Stat Double1Dec))
    , _slSunshineSecs96St  :: !(Maybe (Sum Int))
    , _slSunshineSecs120St :: !(Maybe (Sum Int))
    , _slSunshineSecs144St :: !(Maybe (Sum Int))
    , _slZenithSt          :: !(Maybe (Mean Double1Dec))  -- TODO: is it correct to just average the zenith angle?
    } deriving (Show, Eq, Ord)

makeLenses ''SlStats

instance ToNamedRecord (Maybe SlStats) where
    toNamedRecord a =
        unions
            [ namedRecord
                [ "seconds dni exceeding 96 W/sq m"  .= (fmap getSum . _slSunshineSecs96St  =<< a)
                , "seconds dni exceeding 120 W/sq m" .= (fmap getSum . _slSunshineSecs120St =<< a)
                , "seconds dni exceeding 144 W/sq m" .= (fmap getSum . _slSunshineSecs144St =<< a)
                ]
            , statRecord "ghi"         (_slGhiSt    =<< a)
            , statRecord "dni"         (_slDniSt    =<< a)
            , statRecord "diffuse"     (_slDiffSt   =<< a)
            , statRecord "terrestrial" (_slTerrSt   =<< a)
            , statRecord "dhi"         (_slDhiSt    =<< a)
            , meanRecord "zenith"      (_slZenithSt =<< a)
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
        -- , "local std time"
        -- , "utc time"

        , "ghi mean"
        , "ghi count"
        , "ghi fill count"
        , "ghi max"
        , "ghi min"

        , "dni mean"
        , "dni count"
        , "dni fill count"
        , "dni max"
        , "dni min"

        , "dhi mean"
        , "dhi count"
        , "dhi fill count"
        , "dhi max"
        , "dhi min"

        , "diffuse mean"
        , "diffuse count"
        , "diffuse fill count"
        , "diffuse max"
        , "diffuse min"

        , "terrestrial mean"
        , "terrestrial count"
        , "terrestrial fill count"
        , "terrestrial max"
        , "terrestrial min"

        , "seconds dni exceeding 120 W/sq m"
        , "seconds dni exceeding 144 W/sq m"
        , "seconds dni exceeding 96 W/sq m"

        , "zenith mean"
        , "zenith count"
        , "zenith fill count"

        , "air temp mean"
        , "air temp count"
        , "air temp fill count"
        , "air temp max"
        , "air temp min"

        , "wet bulb mean"
        , "wet bulb count"
        , "wet bulb fill count"
        , "wet bulb max"
        , "wet bulb min"

        , "dew point mean"
        , "dew point count"
        , "dew point fill count"
        , "dew point max"
        , "dew point min"

        , "relative humidity mean"
        , "relative humidity count"
        , "relative humidity fill count"
        , "relative humidity max"
        , "relative humidity min"

        , "wind speed mean"
        , "wind speed count"
        , "wind speed fill count"
        , "wind speed max"
        , "wind speed min"

        , "wind direction"

        , "precipitation"

        , "msl pressure mean"
        , "msl pressure count"
        , "msl pressure fill count"

        , "qnh pressure mean"
        , "qnh pressure fill count"

        , "station level pressure mean"
        , "station level pressure count"
        , "station level pressure fill count"

        , "visibility mean"
        , "visibility count"
        , "visibility fill count"
        ]

instance ToNamedRecord AwSlCombined where
    toNamedRecord (AwSlCombined Nothing Nothing) = error "We should never have a completely empty record."
    toNamedRecord (AwSlCombined aw sl) =
        unions
            [ namedRecord
                [ "station"    .= ((awStationNumSt <$> aw) <|> (slStationNumSt <$> sl))
                , "local time" .= ((awLTimeSt      <$> aw) <|> (slLTimeSt      <$> sl))
                ]
            , toNamedRecord aw
            , toNamedRecord sl
            ]

data SolarSat = SolarSat
  {_ssTime :: UTCTime
  ,_ssVal  :: !(Maybe Double1Dec)
  } deriving (Show, Eq, Ord, Generic )

makeLenses ''SolarSat

instance FromNamedRecord SolarSat where
  parseNamedRecord r = SolarSat
    <$> (r .: "Time (AEST)" >>= parseTimeM True defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%Z")))
    <*> (r .: "MW")

instance FromRecord SolarSat where
  parseRecord v = SolarSat
    <$> (v .!! 0 >>= parseTimeM True defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%Z")))
    <*> (v .!! 1 <|> pure Nothing)

data BoMStation = BoMStation
    {                                                       -- ignoring col hm
    _bsStationNum            :: !(Spaced Text)               -- Station Number,
    , _bsLTime               :: !LTime                       -- Year Month Day Hour Minutes in YYYY, MM, DD, HH24, MI format in Local time,
    , _bsLocalStdTime        :: !LTime                       -- Year Month Day Hour Minutes in YYYY, MM, DD, HH24, MI format in Local standard time,
    , _bsPrecip              :: !(Spaced (Maybe Double1Dec)) -- Precipitation since 9am local time in mm,
    , _bsPrecipQual          :: !(Spaced (Maybe Char))       -- Quality of precipitation since 9am local time,
    , _bsAirTemp             :: !(Spaced (Maybe Double1Dec)) -- Air Temperature in degrees C,
    , _bsAirTempQual         :: !(Spaced (Maybe Char))       -- Quality of air temperature,
    , _bsWetBulbTemp         :: !(Spaced (Maybe Double1Dec)) -- Wet bulb temperature in degrees C,
    , _bsWetBulbTempQual     :: !(Spaced (Maybe Char))       -- Quality of Wet bulb temperature,
    , _bsDewPoint            :: !(Spaced (Maybe Double1Dec)) -- Dew point temperature in degrees C,
    , _bsDewPointQual        :: !(Spaced (Maybe Char))       -- Quality of dew point temperature,
    , _bsRelHumid            :: !(Spaced (Maybe Double1Dec)) -- Relative humidity in percentage %,
    , _bsRelHumidQual        :: !(Spaced (Maybe Char))       -- Quality of relative humidity,
    , _bsVapourPres          :: !(Spaced (Maybe Double1Dec)) -- Vapour pressure in hPa,
    , _bsVapourPresQual      :: !(Spaced (Maybe Char))       -- Quality of vapour pressure,
    , _bsSatVapourPres       :: !(Spaced (Maybe Double1Dec)) -- Saturated vapour pressure in hPa,
    , _bsSatVapourPresQual   :: !(Spaced (Maybe Char))       -- Quality of saturated vapour pressure,
    , _bsWindSpeed           :: !(Spaced (Maybe Double1Dec)) -- Wind speed in km/h,
    , _bsWindSpeedQual       :: !(Spaced (Maybe Char))       -- Wind speed quality,
    , _bsWindDir             :: !(Spaced (Maybe Int))        -- Wind direction in degrees true,
    , _bsWindDirQual         :: !(Spaced (Maybe Char))       -- Wind direction quality,
    , _bsWindGustMax         :: !(Spaced (Maybe Double1Dec)) -- Speed of maximum windgust in last 10 minutes in  km/h,
    , _bsWindGustMaxQual     :: !(Spaced (Maybe Char))       -- Quality of speed of maximum windgust in last 10 minutes,
    , _bsSeaLevPress         :: !(Spaced (Maybe Double1Dec)) -- Mean sea level pressure in hPa,
    , _bsSeaLevPressQual     :: !(Spaced (Maybe Char))       -- Quality of mean sea level pressure,
    , _bsAWSFlag             :: !(Spaced (Maybe Int))        -- AWS Flag,
    , _bsUTCTime             :: !(Maybe UTCTime)
    } deriving (Show, Eq, Ord, Generic)

makeLenses ''BoMStation

bomStationStdTime :: Getter BoMStation (Text,LocalTime)
bomStationStdTime = to (\bs -> (bs ^. bsStationNum . to unSpaced,bs ^. bsLocalStdTime . to unLTime))

instance FromRecord BoMStation where
    parseRecord v
        | V.length v /= 36 = fail $ "Expected 36 columns, found " ++ show (V.length v)
        | otherwise = BoMStation
          <$> v .!! 1            -- Station Number,
          <*> fieldsToLTime 2 v -- Year Month Day Hour Minutes in YYYY, MM, DD, HH24, MI format in Local time,
          <*> fieldsToLTime 7 v -- Year Month Day Hour Minutes in YYYY, MM, DD, HH24, MI format in Local standard time,
          <*> v .!! 12           -- Precipitation since 9am local time in mm,
          <*> v .!! 13           -- Quality of precipitation since 9am local time,
          <*> v .!! 14           -- Air Temperature in degrees C,
          <*> v .!! 15           -- Quality of air temperature,
          <*> v .!! 16           -- Wet bulb temperature in degrees C,
          <*> v .!! 17           -- Quality of Wet bulb temperature,
          <*> v .!! 18           -- Dew point temperature in degrees C,
          <*> v .!! 19           -- Quality of dew point temperature,
          <*> (v .!! 20          -- Relative humidity in percentage %,
              <|> pure (Spaced Nothing)) -- Sometimes there's '###' entries
          <*> v .!! 21           -- Quality of relative humidity,
          <*> v .!! 22           -- Vapour pressure in hPa,
          <*> v .!! 23           -- Quality of vapour pressure,
          <*> v .!! 24           -- Saturated vapour pressure in hPa,
          <*> v .!! 25           -- Quality of saturated vapour pressure,
          <*> (v .!! 26          -- Wind speed in km/h,
              <|> pure (Spaced Nothing)) -- Sometimes there's '###' entries
          <*> v .!! 27           -- Wind speed quality,
          <*> v .!! 28           -- Wind direction in degrees true,
          <*> v .!! 29           -- Wind direction quality,
          <*> (v .!! 30           -- Speed of maximum windgust in last 10 minutes in  km/h,
              <|> pure (Spaced Nothing)) -- Sometimes there's '###' entries
          <*> v .!! 31           -- Quality of speed of maximum windgust in last 10 minutes,
          <*> v .!! 32           -- Mean sea level pressure in hPa,
          <*> v .!! 33           -- Quality of mean sea level pressure,
          <*> v .!! 34           -- AWS Flag,
          <*> pure Nothing

{-
hd,
Station Number,
 Year Month Day Hour Minutes in YYYY, MM, DD, HH24, MI format in Local time,
 Year Month Day Hour Minutes in YYYY, MM, DD, HH24, MI format in Local standard time,
Average air temperature in last 30 minutes in degrees Celsius where observations count >= 12,
Quality of average air temperature in last 30 minutes,
Count of average air temperature observations in last 30 minutes,
Relative humidity in percentage %,
Quality of relative humidity,
Average wind speed in last 30 minutes in km/h where observations count >= 12,
Quality of average wind speed in last 30 minutes,
Count of average wind speed observations in last 30 minutes,
Highest maximum 3 sec wind gust in last 30 minutes in km/h where observations count >= 12,
Quality of Highest maximum 3 sec wind gust in last 30 minutes,
Count of Highest maximum 3 sec wind gust observations in last 30 minutes,
Average direction of wind in last 30 minutes in degrees true where observations count >= 12,
Quality of average direction of wind in last 30 minutes,
Count of average direction of wind observations in last 30 minutes,
#

-}

data BoMAveStation = BoMAveStation
  {
  _basStationNum        :: !(Spaced Text)                --  Station Number,
  , _basLTime           :: !LTime                        --  Year Month Day Hour Minutes in YYYY, MM, DD, HH24, MI format in Local time,
  , _basLocalStdTime    :: !LTime                        --  Year Month Day Hour Minutes in YYYY, MM, DD, HH24, MI format in Local standard time,
  , _basAirTemp         :: !(Spaced (Maybe Double1Dec))  -- Average air temperature in last 30 minutes in degrees Celsius where observations count >= 12,
  , _basAirTempQual     :: !(Spaced Char)                -- Quality of average air temperature in last 30 minutes,
  , _basAirTempCount    :: !(Spaced Int)                 -- Count of average air temperature observations in last 30 minutes,
  , _basHumidPct        :: !(Spaced (Maybe Double1Dec))  -- Relative humidity in percentage %,
  , _basHumidPctQual    :: !(Spaced Char)                -- Quality of relative humidity,
  , _basWindSpeed       :: !(Spaced (Maybe Double1Dec))  -- Average wind speed in last 30 minutes in km/h where observations count >= 12,
  , _basWindSpeedQual   :: !(Spaced Char)                -- Quality of average wind speed in last 30 minutes,
  , _basWindSpeedCount  :: !(Spaced Int)                 -- Count of average wind speed observations in last 30 minutes,
  , _basGustSpeed       :: !(Spaced (Maybe Double1Dec))  -- Highest maximum 3 sec wind gust in last 30 minutes in km/h where observations count >= 12,
  , _basGustSpeedQual   :: !(Spaced Char)                -- Quality of Highest maximum 3 sec wind gust in last 30 minutes,
  , _basGustSpeedCount  :: !(Spaced Int)                 -- Count of Highest maximum 3 sec wind gust observations in last 30 minutes,
  , _basWindDir         :: !(Spaced (Maybe Double1Dec))  -- Average direction of wind in last 30 minutes in degrees true where observations count >= 12,
  , _basWindDirQual     :: !(Spaced Char)                -- Quality of average direction of wind in last 30 minutes,
  , _basWindDirCount    :: !(Spaced Int)                 -- Count of average direction of wind observations in last 30 minutes,
  , _basUTCTime         :: !(Maybe UTCTime)
  }

makeLenses ''BoMAveStation

bomAveStationStdTime :: Getter BoMAveStation (Text,LocalTime)
bomAveStationStdTime = to (\bas -> (bas ^. basStationNum . to unSpaced,bas ^. basLocalStdTime . to unLTime))

szero :: Parser (Spaced Int)
szero = pure (Spaced 0)

sspace :: Parser (Spaced Char)
sspace = pure (Spaced ' ')

instance FromRecord BoMAveStation where
  parseRecord r
    | V.length r /= 27 = fail $ "Expected 27 columns, found " ++ show (V.length r)
    | otherwise = BoMAveStation
      <$> r .!! 1              -- Station Number,
      <*> fieldsToLTime 2 r   --  Year Month Day Hour Minutes in YYYY, MM, DD, HH24, MI format in Local time,
      <*> fieldsToLTime 7 r   --  Year Month Day Hour Minutes in YYYY, MM, DD, HH24, MI format in Local standard time,
      <*> r .!! 12             -- Average air temperature in last 30 minutes in degrees Celsius where observations count >= 12,
      <*> (r .!! 13 <|> sspace) -- Quality of average air temperature in last 30 minutes,
      <*> (r .!! 14 <|> szero)  -- Count of average air temperature observations in last 30 minutes,
      <*> r .!! 15             -- Relative humidity in percentage %,
      <*> (r .!! 16 <|> sspace) -- Quality of relative humidity,
      <*> r .!! 17             -- Average wind speed in last 30 minutes in km/h where observations count >= 12,
      <*> (r .!! 18 <|> sspace) -- Quality of average wind speed in last 30 minutes,
      <*> (r .!! 19 <|> szero)  -- Count of average wind speed observations in last 30 minutes,
      <*> r .!! 20             -- Highest maximum 3 sec wind gust in last 30 minutes in km/h where observations count >= 12,
      <*> (r .!! 21 <|> sspace)  -- Quality of Highest maximum 3 sec wind gust in last 30 minutes,
      <*> (r .!! 22 <|> szero)  -- Count of Highest maximum 3 sec wind gust observations in last 30 minutes,
      <*> r .!! 23             -- Average direction of wind in last 30 minutes in degrees true where observations count >= 12,
      <*> (r .!! 24  <|> sspace) -- Quality of average direction of wind in last 30 minutes,
      <*> (r .!! 25 <|> szero)  -- Count of average direction of wind observations in last 30 minutes,
      <*> pure Nothing




data StationMeta = StationMeta
  {
  -- | Bureau of Meteorology Station Number.
  _smStationNum     :: !(Spaced Text)
  -- | Rainfall district code
  , _smRainDist      :: !(Spaced Int)
  -- | Station Name.
  , _smStationName   :: !(Spaced Text)
  -- | Month/Year site opened. (MM/YYYY)
  , _smSiteOpen      :: !(Spaced Text)
  -- | Month/Year site closed. (MM/YYYY)
  , _smSiteClosed    :: !(Spaced Text)
  -- | Latitude to 4 decimal places - in decimal degrees.
  , _smLat           :: !(Spaced Double)
  -- | Longitude to 4 decimal places - in decimal degrees.
  , _smLon           :: !(Spaced Double)
  -- | Method by which latitude/longitude was derived.
  , _smLocMethod     :: !(Spaced Text)
  -- | State.
  , _smState         :: !(Spaced Text)
  -- | Height of station above mean sea level in metres.
  , _smHeightASL     :: !(Spaced (Maybe Double))
  -- | Height of barometer above mean sea level in metres.
  , _smHeightBaroASL :: !(Spaced (Maybe Double))
  -- | WMO (World Meteorological Organisation) Index Number.
  , _smWMONum        :: !(Spaced (Maybe Int))
  -- | First year of data supplied in data file.
  , _smFirstYear     :: !(Spaced Int)
  -- | Last year of data supplied in data file.0.
  , _smLastYear      :: !(Spaced Int)
  -- | Percentage complete between first and last records.
  , _smPctComplete   :: !(Spaced Int)
  -- | Percentage of values with quality flag 'Y'.
  , _smValY          :: !(Spaced Int)
  -- | Percentage of values with quality flag 'N'.
  , _smValN          :: !(Spaced Int)
  -- | Percentage of values with quality flag 'W'.
  , _smValW          :: !(Spaced Int)
  -- | Percentage of values with quality flag 'S'.
  , _smValS          :: !(Spaced Int)
  -- | Percentage of values with quality flag 'I'.
  , _smValI          :: !(Spaced Int)
  , _smUTCTime       :: !(Maybe UTCTime)
  } deriving (Show, Eq, Generic)

makeLenses ''StationMeta

instance FromRecord StationMeta where
  parseRecord r
    | V.length r /= 22 = fail $ "FromRecord StationMeta: Expected 21 columns, found " ++ show (V.length r)
    | otherwise = StationMeta
      <$> r .!! 1                        -- Bureau of Meteorology Station Number.
      <*> r .!! 2                        -- Rainfall district code
      <*> r .!! 3                        -- Station Name.
      <*> r .!! 4                        -- Month/Year site opened. (MM/YYYY)
      <*> r .!! 5                        -- Month/Year site closed. (MM/YYYY)
      <*> r .!! 6                        -- Latitude to 4 decimal places - in decimal degrees.
      <*> r .!! 7                        -- Longitude to 4 decimal places - in decimal degrees.
      <*> r .!! 8                        -- Method by which latitude/longitude was derived.
      <*> r .!! 9                        -- State.
      <*> r .!! 10                       -- Height of station above mean sea level in metres.
      <*> r .!! 11                       -- Height of barometer above mean sea level in metres.
      <*> r .!! 12                       -- WMO (World Meteorological Organisation) Index Number.
      <*> r .!! 13                       -- First year of data supplied in data file.
      <*> r .!! 14                       -- Last year of data supplied in data file.
      <*> r .!! 15                       -- Percentage complete between first and last records.
      <*> (r .!! 16 <|> pure (Spaced 0)) -- Percentage of values with quality flag 'Y'.
      <*> (r .!! 17 <|> pure (Spaced 0)) -- Percentage of values with quality flag 'N'.
      <*> (r .!! 18 <|> pure (Spaced 0)) -- Percentage of values with quality flag 'W'.
      <*> (r .!! 19 <|> pure (Spaced 0)) -- Percentage of values with quality flag 'S'.
      <*> (r .!! 20 <|> pure (Spaced 0)) -- Percentage of values with quality flag 'I'.
      <*> pure Nothing

{-
data TMY3 = TMY3
  { _tmy3Date -- (MM/DD/YYYY) -- Date (MM/DD/YYYY)
  , _tmy3Time -- (HH:MM) -- Time (HH:MM)
  , _tmy3ETR -- (W/m^2) -- ETR (W/m^2)
  , _tmy3ETRN -- (W/m^2) -- ETRN (W/m^2)
  , _tmy3GHI -- (W/m^2) -- GHI (W/m^2)
  , _tmy3GHISource -- -- GHI source
  , _tmy3GHIUncert -- (%) -- GHI uncert (%)
  , _tmy3DNI -- (W/m^2) -- DNI (W/m^2)
  , _tmy3DNISource -- -- DNI source
  , _tmy3DNIUncert -- (%) -- DNI uncert (%)
  , _tmy3DHI -- (W/m^2) -- DHI (W/m^2)
  , _tmy3DHISource -- -- DHI source
  , _tmy3DHIUncert -- (%) -- DHI uncert (%)
  , _tmy3GHIllum -- (lx) -- GH illum (lx)
  , _tmy3GHIllumSource -- -- GH illum source
  , _tmy3GlobalIllumUncert -- (%) -- Global illum uncert (%)
  , _tmy3DNIllum -- (lx) -- DN illum (lx)
  , _tmy3DNIllumSource -- -- DN illum source
  , _tmy3DNIllumUncert -- (%) -- DN illum uncert (%)
  , _tmy3DHIllum -- (lx) -- DH illum (lx)
  , _tmy3DHIllumSource -- -- DH illum source
  , _tmy3DHIllumUncert -- (%) -- DH illum uncert (%)
  , _tmy3ZenithLum -- (cd/m^2) -- Zenith lum (cd/m^2)
  , _tmy3ZenithLumSource -- -- Zenith lum source
  , _tmy3ZenithLumUncert -- (%) -- Zenith lum uncert (%)
  , _tmy3TotCld -- (tenths) -- TotCld (tenths)
  , _tmy3TotCldSource -- -- TotCld source
  , _tmy3TotCldUncert -- (code) -- TotCld uncert (code)
  , _tmy3OpqCld -- (tenths) -- OpqCld (tenths)
  , _tmy3OpqCldSource -- -- OpqCld source
  , _tmy3OpqCldUncert -- (code) -- OpqCld uncert (code)
  , _tmy3Dry ---bulb (C) -- Dry-bulb (C)
  , _tmy3Dry ---bulbSource -- Dry-bulb source
  , _tmy3Dry ---bulbUncert (code) -- Dry-bulb uncert (code)
  , _tmy3Dew ---point (C) -- Dew-point (C)
  , _tmy3Dew ---pointSource -- Dew-point source
  , _tmy3Dew ---pointUncert (code) -- Dew-point uncert (code)
  , _tmy3RHum -- (%) -- RHum (%)
  , _tmy3RHumSource -- -- RHum source
  , _tmy3RHumUncert -- (code) -- RHum uncert (code)
  , _tmy3Pressure -- (mbar) -- Pressure (mbar)
  , _tmy3PressureSource -- -- Pressure source
  , _tmy3PressureUncert -- (code) -- Pressure uncert (code)
  , _tmy3Wdir -- (degrees) -- Wdir (degrees)
  , _tmy3WdirSource -- -- Wdir source
  , _tmy3WdirUncert -- (code) -- Wdir uncert (code)
  , _tmy3Wspd -- (m/s) -- Wspd (m/s)
  , _tmy3WspdSource -- -- Wspd source
  , _tmy3WspdUncert -- (code) -- Wspd uncert (code)
  , _tmy3Hvis -- (m) -- Hvis (m)
  , _tmy3HvisSource -- -- Hvis source
  , _tmy3HvisUncert -- (code) -- Hvis uncert (code)
  , _tmy3CeilHgt -- (m) -- CeilHgt (m)
  , _tmy3CeilHgtSource -- -- CeilHgt source
  , _tmy3CeilHgtUncert -- (code) -- CeilHgt uncert (code)
  , _tmy3Pwat -- (cm) -- Pwat (cm)
  , _tmy3PwatSource -- -- Pwat source
  , _tmy3PwatUncert -- (code) -- Pwat uncert (code)
  , _tmy3AOD -- (unitless) -- AOD (unitless)
  , _tmy3AODSource -- -- AOD source
  , _tmy3AODUncert -- (code) -- AOD uncert (code)
  , _tmy3Alb -- (unitless) -- Alb (unitless)
  , _tmy3AlbSource -- -- Alb source
  , _tmy3AlbUncert -- (code) -- Alb uncert (code)
  , _tmy3LprecipDepth -- (mm) -- Lprecip depth (mm)
  , _tmy3LprecipQuantity -- (hr) -- Lprecip quantity (hr)
  , _tmy3LprecipSource -- -- Lprecip source
  , _tmy3LprecipUncert -- (code) -- Lprecip uncert (code)
  , _tmy3PresWth -- (METAR code) -- PresWth (METAR code)
  , _tmy3PresWthSource -- -- PresWth source
  , _tmy3PresWthUncert -- (code) -- PresWth uncert (code)

}
-}

data Processing recType = Processing
    { lTime    :: recType -> LocalTime
    , setLTime :: recType -> LocalTime -> recType
    , stNum    :: recType -> Text
    , mkEmpty  :: Text    -> LocalTime -> recType
    }

data FieldType ftype = FieldType
    { mkValue  :: Double1Dec -> ftype
    , getValue :: ftype      -> Double1Dec
    }


type Processor = forall a b. (Show a, Show b) => Processing a -> (Lens' a (Maybe b)) -> FieldType b -> [a] -> [a]
