{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

import ObsTmy


data Test = Test
    { foo    :: !String
    , bar    :: !Int
    , woo    :: !Double
    , wibble :: !String
    }


instance FromNamedRecord Test where
    parseNamedRecord r = Test <$> r .: "Foo" 
                             <*> r .: "Bar" 
                             <*> r .: "Woo" 
                             <*> r .: "Wibble"


main :: IO ()
main = do
    csvData <- BL.readFile "test.csv"
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v $ \ t ->
            putStrLn $ foo t ++ "; " ++ show (bar t) ++ "; " ++ show (woo t) ++ "; " ++ wibble t
