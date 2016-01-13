{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tmy.Csv where

import Control.Concurrent.Async             (concurrently)
import Control.Monad                        (mplus)
import qualified Data.ByteString      as B  (span, spanEnd)
import qualified Data.ByteString.Lazy as BL (readFile, empty)
import Data.Csv                      hiding (decodeByName, decode)
import Data.Csv.Streaming                   (Records(Cons, Nil), decodeByName, decode)
import Data.Time.Calendar                   (fromGregorianValid)
import Data.Time.Format                     (formatTime)
import Data.Time.LocalTime                  (LocalTime(LocalTime), makeTimeOfDayValid)
import Data.Time.Locale.Compat              (iso8601DateFormat, defaultTimeLocale)
import Text.Printf                          (printf)


newtype Spaced a = Spaced {unSpaced :: a} deriving (Show, Eq, Ord, ToField)

instance FromField a => FromField (Spaced a) where
    -- filter out leading and trailing spaces
    parseField bs = Spaced <$> parseField (fst . B.spanEnd (== 32) . snd . B.span (== 32) $ bs)


newtype Double1Dec = Double1Dec Double deriving (Show, Eq, Ord, FromField, Num, Fractional)

instance ToField Double1Dec where
    toField (Double1Dec d) = toField (printf "%.1f" d :: String)


newtype LTime = LTime { unLTime :: LocalTime } deriving (Eq, Show, Ord)

instance ToField LTime where
    toField (LTime lt) = toField (formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) lt)


concatRecs :: [Records a] -> [a]
concatRecs [] = []
concatRecs (x:xs) = go x where
    go (Nil  (Just e)  _)  = error e
    go (Nil  Nothing   _)  = concatRecs xs
    go (Cons (Right a) rs) = a : go rs
    go (Cons (Left e)  _)  = error e


-- | For parsing LTime from a CSV with columns like:
--   'Year Month Day Hours Minutes in YYYY, MM, DD, HH24, MI format in Local time'
fieldsToLTime :: Int -> Record -> Parser LTime
fieldsToLTime i v =
    mplus (colsToLTime (v .! i) (v .! (i+1)) (v .! (i+2)) (v .! (i+3)) (v .! (i+4)))
          (fail $ "Could not parse date starting at col " ++ show i)


colsToLTime :: Parser Integer -> Parser Int -> Parser Int -> Parser Int -> Parser Int -> Parser LTime
colsToLTime y m d h mn = do
    mlt <- maybeLTime <$> y <*> m <*> d <*> h <*> mn
    maybe (fail "Could not parse date") return mlt


maybeLTime :: Integer -> Int -> Int -> Int -> Int -> Maybe LTime
maybeLTime y m d h mn = LTime <$> (LocalTime
                                    <$> fromGregorianValid y m d
                                    <*> makeTimeOfDayValid h mn 0)


mapRecords_ :: (a -> IO ()) -> Records a -> IO ()
mapRecords_ f (Cons eith rs) = do
    _ <- concurrently
            (either (printFailure "Record failed: ") f eith)
            (mapRecords_ f rs)
    return ()
mapRecords_ _ (Nil err _) =
    maybe (return ()) (printFailure "Failed to parse: ") err


readCsv :: FromNamedRecord a => FilePath -> IO (Records a)
readCsv fn = do
    bs <- BL.readFile fn
    case decodeByName bs of
        Left err -> do
            putStrLn ("Failed to read file '" ++ fn ++ "': " ++ err)
            return (Nil Nothing BL.empty)
        Right (_, rs) -> return rs


readIndexedCsv :: FromRecord a => FilePath -> IO (Records a)
readIndexedCsv fn = do
    bs <- BL.readFile fn
    return (decode HasHeader bs)


printFailure :: String -> String -> IO ()
printFailure pre err = putStrLn (pre ++ err)
