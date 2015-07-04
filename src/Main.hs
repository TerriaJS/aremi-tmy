{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad                        (liftM)
import qualified Data.ByteString.Lazy as BL (ByteString, readFile)
import Data.Csv                             (Header, decodeByName, (.:))
import qualified Data.Vector          as V  (Vector, mapM_, concat)
import System.Environment                   (getArgs)

import Tmy.Import


main :: IO ()
main = do
    args <- getArgs
    vs <- liftM V.concat (mapM parseCsv args)
    V.mapM_ print vs


parseCsv :: String -> IO (V.Vector OneMinSolarSite)
parseCsv fn = do
    bs <- BL.readFile fn
    case decodeByName bs of
        Left err -> error err
        Right (_, v) -> return v

