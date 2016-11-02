{-# LANGUAGE OverloadedStrings #-}

module Tmy.OneMinSolar.TimeZones
    ( getTZ
    ) where

import Data.HashMap.Strict as M
import Data.Text as T

import Control.Applicative

import Data.Time.LocalTime (minutesToTimeZone, TimeZone)

h :: Int -> Int -> Int
h hours mins = hours*60 + mins

knownTZs :: M.HashMap Text Int
knownTZs = M.fromList
  [("200839",10`h` 30)
  ,("200288",11`h`  0)
  ,("047048", 9`h` 30)
  ]

stateTZs :: M.HashMap Text Int
stateTZs = M.fromList
  [("NSW", 10`h`  0)
  ,("NT" ,  9`h` 30)
  ,("QLD", 10`h`  0)
  ,("SA" ,  9`h` 30)
  ,("TAS", 10`h`  0)
  ,("VIC", 10`h`  0)
  ,("WA" ,  8`h`  0)
  ]

-- | Given a station number and a state, returns the TimeZone for that station
getTZ :: Text -> Text -> Maybe TimeZone
getTZ station state = minutesToTimeZone <$> (M.lookup station knownTZs <|> M.lookup state stateTZs)
