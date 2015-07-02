{-# LANGUAGE OverloadedStrings #-}

module ObsTmy.Import where

import Control.Applicative                  ((<$>), (<*>))
import Data.Csv                             (FromNamedRecord, parseNamedRecord, (.:))
import Data.Text                            (Text, strip)


data Site = Site
    { foo    :: !Text
    , bar    :: !Int
    , woo    :: !Double
    , wibble :: !Text
    } deriving (Show, Eq, Ord)


instance FromNamedRecord Site where
    parseNamedRecord r = Site <$> r .: "Foo" 
                              <*> r .: "Bar" 
                              <*> r .: "Woo" 
                              <*> r .: "Wibble"
