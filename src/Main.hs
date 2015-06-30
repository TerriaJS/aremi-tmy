{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative                  ((<$>), (<*>))
import qualified Data.ByteString.Lazy as BL (readFile)
import Data.Csv                             (FromNamedRecord, parseNamedRecord, decodeByName, (.:))
import qualified Data.Vector          as V  (forM_)
import System.Environment                   (getArgs)

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
    args <- getArgs
    mapM_ printCsv args


printCsv :: String -> IO ()
printCsv fn = do
    csvData <- BL.readFile fn
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v $ \ t ->
            putStrLn $ foo t ++ "; " ++ show (bar t) ++ "; " ++ show (woo t) ++ "; " ++ wibble t
