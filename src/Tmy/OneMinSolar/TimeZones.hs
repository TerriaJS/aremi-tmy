{-# LANGUAGE OverloadedStrings #-}

module Tmy.OneMinSolar.TimeZones
    ( getTZ
    ) where

import Data.HashMap.Strict as M
import Data.Text as T

import Control.Applicative

import Data.Time.LocalTime (minutesToTimeZone, TimeZone)

knownTZs :: M.HashMap Text Int
knownTZs = M.fromList
  [("200839",10*60 + 30)
  ,("200288",11*60 +  0)
  ,("047048", 9*60 + 30)
  ]

stateTZs :: M.HashMap Text Int
stateTZs = M.fromList
  [("NSW",10*60 +  0)
  ,("NT" , 9*60 + 30)
  ,("QLD",10*60 +  0)
  ,("SA" , 9*60 + 30)
  ,("TAS",10*60 +  0)
  ,("VIC",10*60 +  0)
  ,("WA" , 8*60 +  0)
  ]

-- | Given a station number and a state, returns the TimeZone for that station
getTZ :: Text -> Text -> Maybe TimeZone
getTZ station state = minutesToTimeZone <$> (M.lookup station knownTZs <|> M.lookup state stateTZs)
