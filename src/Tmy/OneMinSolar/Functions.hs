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


awAggrToHour :: AwStats -> AwStats -> AwStats
awAggrToHour a b =
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
        , awWindDirSt         = Just 0 -- TODO: vector math
        , awVisibilitySt      = combine awVisibilitySt      a b
        , awMslPressSt        = combine awMslPressSt        a b
        , awStationLvlPressSt = combine awStationLvlPressSt a b
        , awQnhPressSt        = combine awQnhPressSt        a b
        }


slAggrToHour :: SlStats -> SlStats -> SlStats
slAggrToHour a b =
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
        , awWindDirSt         = Just 0 -- TODO: vector math
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


maybeQualStat :: (a -> Spaced Char)
              -> (a -> Spaced (Maybe b))
              -> (a -> Spaced (Maybe b))
              -> (a -> Spaced (Maybe b))
              -> a
              -> Maybe (Stat b)
maybeQualStat meanQf meanF maxF minF a =
    case qFilter meanQf meanF a of
        Just _  -> maybeStat meanF maxF minF a
        Nothing -> Nothing


qFilter :: (a -> Spaced Char)
        -> (a -> Spaced (Maybe b))
        -> a
        -> Maybe b
qFilter qf vf a =
    if unSpaced (qf a) `elem` "YNSF"
        then unSpaced (vf a)
        else Nothing


