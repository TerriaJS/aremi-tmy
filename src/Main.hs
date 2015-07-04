{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL (ByteString, readFile, empty)
import Data.Csv                             (Header)
import Data.Csv.Streaming                   (Records(Cons, Nil), decodeByName)
import System.Environment                   (getArgs)

import Tmy.Import


main :: IO ()
main = do
    args <- getArgs
    rs <- mapM parseCsv args
    mapM_ print1minSolarSite rs


parseCsv :: String -> IO (Records OneMinSolarSite)
parseCsv fn = do
    bs <- BL.readFile fn
    case decodeByName bs of
        Left err -> do
            putStrLn ("Failed to read file '" ++ fn ++ "': " ++ err)
            return (Nil Nothing BL.empty)
        Right (_, rs) -> return rs


print1minSolarSite :: Records OneMinSolarSite -> IO ()
print1minSolarSite (Cons ei rs) = do
    either printFailure print ei
    print1minSolarSite rs
print1minSolarSite (Nil Nothing _) =
    putStrLn "All done."
print1minSolarSite (Nil (Just err) _) =
    printFailure err


printFailure :: String -> IO ()
printFailure err = putStrLn ("Record failed: " ++ err)
