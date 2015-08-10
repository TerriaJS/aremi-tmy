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
        { awStationNumSt       = awStationNumSt   a
        , awLTimeSt            = floorMinute (awLTimeSt a)
        , awLocalStdTimeSt     = awLocalStdTimeSt a
        , awUtcTimeSt          = awUtcTimeSt      a
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
        , awLocalStdTimeSt     = awLocalStdTime a
        , awUtcTimeSt          = awUtcTime      a
        , _awAirTempSt         = maybeQualStat awAirTempQual      awAirTemp      awAirTempMax      awAirTempMin      a
        , _awWetBulbTempSt     = maybeQualStat awWetBulbTempQual  awWetBulbTemp  awWetBulbTempMax  awWetBulbTempMin  a
        , _awDewPointTempSt    = maybeQualStat awDewPointTempQual awDewPointTemp awDewPointTempMax awDewPointTempMin a
        , _awRelHumidSt        = maybeQualStat awRelHumidQual     awRelHumid     awRelHumidMax     awRelHumidMin     a
        , _awWindSpeedSt       = maybeQualStat awWindSpeedQual    awWindSpeed    awWindGustMax     awWindSpeedMin    a
        , _awPrecipSinceLastSt = Sum <$> qFilter awPrecipQual  awPrecipSinceLast a
        , awWindDirSt          = qFilter awWindDirQual awWindDir a
        , _awVisibilitySt      = mkSumCount <$> qFilter awVisibilityQual      awVisibility      a
        , _awMslPressSt        = mkSumCount <$> qFilter awMslPressQual        awMslPress        a
        , _awStationLvlPressSt = mkSumCount <$> qFilter awStationLvlPressQual awStationLvlPress a
        , _awQnhPressSt        = mkSumCount <$> qFilter awQnhPressQual        awQnhPress        a
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
        , _slZenithSt          = mkSumCount <$> unSpaced (slZenith a)
        }

