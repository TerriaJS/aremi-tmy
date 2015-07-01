{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative                  ((<$>), (<*>))
import Control.Monad                        (liftM)
import qualified Data.ByteString.Lazy as BL (ByteString, readFile)
import Data.Csv                             (FromNamedRecord, Header, parseNamedRecord, decodeByName, (.:))
import qualified Data.Vector          as V  (Vector, mapM_, concat)
import System.Environment                   (getArgs)

import ObsTmy


data Test = Test
    { foo    :: !String
    , bar    :: !Int
    , woo    :: !Double
    , wibble :: !String
    } deriving (Show, Eq, Ord)


instance FromNamedRecord Test where
    parseNamedRecord r = Test <$> r .: "Foo" 
                              <*> r .: "Bar" 
                              <*> r .: "Woo" 
                              <*> r .: "Wibble"


main :: IO ()
main = do
    args <- getArgs
    vs <- liftM V.concat (mapM parseCsv args)
    V.mapM_ print vs


parseCsv :: String -> IO (V.Vector Test)
parseCsv fn = do
    bs <- BL.readFile fn
    case decodeByName bs of
        Left err -> undefined
        --Right (_, v) -> V.forM v $ \ t -> return $ foo t ++ "; " ++ show (bar t) ++ "; " ++ show (woo t) ++ "; " ++ wibble t
        Right (_, v) -> return v

