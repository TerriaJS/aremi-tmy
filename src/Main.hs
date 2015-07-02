{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative                  ((<$>), (<*>))
import Control.Monad                        (liftM)
import qualified Data.ByteString.Lazy as BL (ByteString, readFile)
import Data.Csv                             (FromNamedRecord, Header, parseNamedRecord, decodeByName, (.:))
import Data.Text                            (Text, strip)
import qualified Data.Vector          as V  (Vector, mapM_, concat)
import System.Environment                   (getArgs)

import ObsTmy.Import


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


main :: IO ()
main = do
    args <- getArgs
    vs <- liftM V.concat (mapM parseCsv args)
    V.mapM_ print vs


parseCsv :: String -> IO (V.Vector Site)
parseCsv fn = do
    bs <- BL.readFile fn
    case decodeByName bs of
        Left err -> undefined
        Right (_, v) -> return v

