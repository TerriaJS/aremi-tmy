module Tmy.OneMinSolar.Functions where


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
        { awStationNumSt      = awStationNumSt   a
        , awLTimeSt           = floorMinute (awLTimeSt a)
        , awLocalStdTimeSt    = awLocalStdTimeSt a
        , awUtcTimeSt         = awUtcTimeSt      a
        , awAirTempSt         = combine awAirTempSt         a b
        , awWetBulbTempSt     = combine awWetBulbTempSt     a b
        , awDewPointTempSt    = combine awDewPointTempSt    a b
        , awRelHumidSt        = combine awRelHumidSt        a b
        , awWindSpeedSt       = combine awWindSpeedSt       a b
        , awPrecipSinceLastSt = combine awPrecipSinceLastSt a b
        -- only keep the wind direction if the minute is 00 - the Sandia method specifies wind
        --   dir should be at the time indicated
        , awWindDirSt         = if minute (awLTimeSt a) == 0 then awWindDirSt a else Nothing
        , awVisibilitySt      = combine awVisibilitySt      a b
        , awMslPressSt        = combine awMslPressSt        a b
        , awStationLvlPressSt = combine awStationLvlPressSt a b
        , awQnhPressSt        = combine awQnhPressSt        a b
        }


slAggr :: SlStats -> SlStats -> SlStats
slAggr a b =
    SlStats
        { slStationNumSt      = slStationNumSt              a
        , slLTimeSt           = floorMinute (slLTimeSt  a)
        , slGhiSt             = combine slGhiSt             a b
        , slDniSt             = combine slDniSt             a b
        , slDiffSt            = combine slDiffSt            a b
        , slTerrSt            = combine slTerrSt            a b
        , slDhiSt             = combine slDhiSt             a b
        , slSunshineSecs96St  = combine slSunshineSecs96St  a b
        , slSunshineSecs120St = combine slSunshineSecs120St a b
        , slSunshineSecs144St = combine slSunshineSecs144St a b
        , slZenithSt          = combine slZenithSt          a b
        }


awToStat :: AutoWeatherObs -> AwStats
awToStat a =
    AwStats
        { awStationNumSt      = unSpaced (awStationNum a)
        , awLTimeSt           = awLTime    a
        , awLocalStdTimeSt    = awLocalStdTime a
        , awUtcTimeSt         = awUtcTime      a
        , awAirTempSt         = maybeQualStat awAirTempQual      awAirTemp      awAirTempMax      awAirTempMin      a
        , awWetBulbTempSt     = maybeQualStat awWetBulbTempQual  awWetBulbTemp  awWetBulbTempMax  awWetBulbTempMin  a
        , awDewPointTempSt    = maybeQualStat awDewPointTempQual awDewPointTemp awDewPointTempMax awDewPointTempMin a
        , awRelHumidSt        = maybeQualStat awRelHumidQual     awRelHumid     awRelHumidMax     awRelHumidMin     a
        , awWindSpeedSt       = maybeQualStat awWindSpeedQual    awWindSpeed    awWindGustMax     awWindSpeedMin    a
        , awPrecipSinceLastSt = Sum <$> qFilter awPrecipQual  awPrecipSinceLast a
        , awWindDirSt         = qFilter awWindDirQual awWindDir a
        , awVisibilitySt      = mkSumCount <$> qFilter awVisibilityQual      awVisibility      a
        , awMslPressSt        = mkSumCount <$> qFilter awMslPressQual        awMslPress        a
        , awStationLvlPressSt = mkSumCount <$> qFilter awStationLvlPressQual awStationLvlPress a
        , awQnhPressSt        = mkSumCount <$> qFilter awQnhPressQual        awQnhPress        a
        }


slToStat :: SolarRadiationObs -> SlStats
slToStat a =
    SlStats
        { slStationNumSt      = unSpaced (slStationNum a)
        , slLTimeSt           = slLTime a
        , slGhiSt             = maybeStat slGhiMean  slGhiMax  slGhiMin  a
        , slDniSt             = maybeStat slDniMean  slDniMax  slDniMin  a
        , slDiffSt            = maybeStat slDiffMean slDiffMax slDiffMin a
        , slTerrSt            = maybeStat slTerrMean slTerrMax slTerrMin a
        , slDhiSt             = maybeStat slDhiMean  slDhiMax  slDhiMin  a
        , slSunshineSecs96St  = Sum <$> unSpaced (slSunshineSecs96  a)
        , slSunshineSecs120St = Sum <$> unSpaced (slSunshineSecs120 a)
        , slSunshineSecs144St = Sum <$> unSpaced (slSunshineSecs144 a)
        , slZenithSt          = mkSumCount <$> unSpaced (slZenith a)
        }

