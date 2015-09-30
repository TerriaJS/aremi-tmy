{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}


module Tmy.OneMinSolar.Functions where

import Control.Lens                         (Lens', (^.), (.~), (&), (+~))
import Data.Maybe                           (fromJust)
import Data.Time.Clock                      (diffUTCTime)
import Data.Time.Lens                       (flexDT, minutes)
import Data.Text                            (Text)
import Data.Time.LocalTime                  (LocalTime, localTimeToUTC, utc)

import Control.Applicative                  ((<$>))
import Data.Semigroup                       (Sum(..))

import Tmy.OneMinSolar.Types
import Tmy.Common
import Tmy.Csv


-- Data file prefix and suffix
awPref :: String
awPref = "aw_"
slPref :: String
slPref = "sl_"
globSuff :: String
globSuff = "*.txt"


awAggr :: AwStats -> AwStats -> AwStats
awAggr a b =
    AwStats
        { awStationNumSt       = awStationNumSt   a
        , awLTimeSt            = floorMinute (awLTimeSt a)
        -- , awLocalStdTimeSt     = awLocalStdTimeSt a
        -- , awUtcTimeSt          = awUtcTimeSt      a
        , _awAirTempSt         = combine _awAirTempSt         a b
        , _awWetBulbTempSt     = combine _awWetBulbTempSt     a b
        , _awDewPointTempSt    = combine _awDewPointTempSt    a b
        , _awRelHumidSt        = combine _awRelHumidSt        a b
        , _awWindSpeedSt       = combine _awWindSpeedSt       a b
        , _awPrecipSinceLastSt = combine _awPrecipSinceLastSt a b
        -- only keep the wind direction if the minute is 00 - the Sandia method specifies wind
        --   dir should be at the time indicated
        , awWindDirSt          = if minute (awLTimeSt a) == 0 then awWindDirSt a else Nothing
        , _awVisibilitySt      = combine _awVisibilitySt      a b
        , _awMslPressSt        = combine _awMslPressSt        a b
        , _awStationLvlPressSt = combine _awStationLvlPressSt a b
        , _awQnhPressSt        = combine _awQnhPressSt        a b
        }


slAggr :: SlStats -> SlStats -> SlStats
slAggr a b =
    SlStats
        { slStationNumSt      = slStationNumSt a
        , slLTimeSt           = floorMinute (slLTimeSt a)
        , _slGhiSt             = combine _slGhiSt             a b
        , _slDniSt             = combine _slDniSt             a b
        , _slDiffSt            = combine _slDiffSt            a b
        , _slTerrSt            = combine _slTerrSt            a b
        , _slDhiSt             = combine _slDhiSt             a b
        , _slSunshineSecs96St  = combine _slSunshineSecs96St  a b
        , _slSunshineSecs120St = combine _slSunshineSecs120St a b
        , _slSunshineSecs144St = combine _slSunshineSecs144St a b
        , _slZenithSt          = combine _slZenithSt          a b
        }


awToStat :: AutoWeatherObs -> AwStats
awToStat a =
    AwStats
        { awStationNumSt       = unSpaced (awStationNum a)
        , awLTimeSt            = awLTime    a
        -- , awLocalStdTimeSt     = awLocalStdTime a
        -- , awUtcTimeSt          = awUtcTime      a
        , _awAirTempSt         = maybeQualStat awAirTempQual      awAirTemp      awAirTempMax      awAirTempMin      a
        , _awWetBulbTempSt     = maybeQualStat awWetBulbTempQual  awWetBulbTemp  awWetBulbTempMax  awWetBulbTempMin  a
        , _awDewPointTempSt    = maybeQualStat awDewPointTempQual awDewPointTemp awDewPointTempMax awDewPointTempMin a
        , _awRelHumidSt        = maybeQualStat awRelHumidQual     awRelHumid     awRelHumidMax     awRelHumidMin     a
        , _awWindSpeedSt       = maybeQualStat awWindSpeedQual    awWindSpeed    awWindGustMax     awWindSpeedMin    a
        , _awPrecipSinceLastSt = Sum <$> qFilter awPrecipQual  awPrecipSinceLast a
        , awWindDirSt          = qFilter awWindDirQual awWindDir a
        , _awVisibilitySt      = mkMean <$> qFilter awVisibilityQual      awVisibility      a
        , _awMslPressSt        = mkMean <$> qFilter awMslPressQual        awMslPress        a
        , _awStationLvlPressSt = mkMean <$> qFilter awStationLvlPressQual awStationLvlPress a
        , _awQnhPressSt        = mkMean <$> qFilter awQnhPressQual        awQnhPress        a
        }


slToStat :: SolarRadiationObs -> SlStats
slToStat a =
    SlStats
        { slStationNumSt       = unSpaced (slStationNum a)
        , slLTimeSt            = slLTime a
        , _slGhiSt             = maybeStat slGhiMean  slGhiMax  slGhiMin  a
        , _slDniSt             = maybeStat slDniMean  slDniMax  slDniMin  a
        , _slDiffSt            = maybeStat slDiffMean slDiffMax slDiffMin a
        , _slTerrSt            = maybeStat slTerrMean slTerrMax slTerrMin a
        , _slDhiSt             = maybeStat slDhiMean  slDhiMax  slDhiMin  a
        , _slSunshineSecs96St  = Sum <$> unSpaced (slSunshineSecs96  a)
        , _slSunshineSecs120St = Sum <$> unSpaced (slSunshineSecs120 a)
        , _slSunshineSecs144St = Sum <$> unSpaced (slSunshineSecs144 a)
        , _slZenithSt          = mkMean <$> unSpaced (slZenith a)
        }


minDiff :: LocalTime -> LocalTime -> Int
minDiff a b = round (diffUTCTime (localTimeToUTC utc a) (localTimeToUTC utc b) / 60)


-- | Find the number of minutes as well as the record that has a Just value for a given field
minutesUntil :: Processing a
             -> (Lens' a (Maybe b))
             -> LocalTime
             -> [a]
             -> Maybe (Int, a)
minutesUntil (Processing{..}) f lt xs = go xs where
    -- check if the field we are interested in has a value
    go (a:as) = case a ^. f of
                    -- if it doesn't, then increment and keep looking
                    Nothing -> go as
                    -- if the field has a value then return the minutes difference and the record
                    Just _  -> Just (minDiff (lTime a) lt, a)
    go [] = Nothing


