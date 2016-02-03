{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}


module Tmy.OneMinSolar.FillAdjacent where

import Control.Applicative                  ((<|>), liftA2)
import Control.Lens                         (Lens', view, (^.), (.~), (&))
import Data.Function                        (on)
import Data.List.NonEmpty                   (NonEmpty(..), (<|), toList)
import qualified Data.List.NonEmpty as NE   (head, last)
import Data.Maybe                           (isJust)
import Data.Time.Clock                      (addUTCTime, NominalDiffTime)
import Data.Time.LocalTime                  (LocalTime, localTimeToUTC, utcToLocalTime,utc)

import Tmy.OneMinSolar.Functions
import Tmy.OneMinSolar.Types


-- |Fill medium sized gaps with data from adjacent days
fillAdjacent :: Processing a
       -> Lens' a (Maybe b)
       -> FieldType b
       -> [a]
       -> [a]
fillAdjacent pr@(Processing{..}) f ft as' = go as' as' where
    go (x:xs) !as'' =
        let as = dropWhile (\y -> lTime y < addDays (-1) (lTime x)) as''
        in case x ^. f of
            -- skip until we have a value
            Nothing -> x : go xs as
            Just _  -> case minutesUntil lTime f (lTime x) xs of
                -- if no gap we must be at the end
                Nothing -> x : xs
                Just (mins, y) ->
                    if isMediumGap mins
                        -- if medium gap then attempt to fill it (else skip)
                        then case getAdjValues pr f ft x y as'' of
                            -- if valid adjacent values exist, update
                            Nothing -> x : go xs as
                            Just vs -> x : go (update vs xs) as
                        else x : go xs as
    go [] _ = []

    -- update the entries in x with entries in v for value f
    update (v:vs) (x:xs)
        -- if time matches update field with value
        | lTime v == lTime x = (x & f .~ (v ^. f)) : update vs xs
        -- if time is missing add new field with value
        | otherwise         = (mkEmpty (stNum v) (lTime v) & f .~ (v ^. f)) : update vs (x:xs)
    update [] xs = xs
    update _ []  = error "Nothing to update" -- []


-- |Optionally get values between the given timestamps shifted by a day
--  and for which all entries of the specified field exist
getAdjValues :: Processing a
             -> Lens' a (Maybe b)
             -> FieldType b
             -> a
             -> a
             -> [a]
             -> Maybe [a]
getAdjValues pr@(Processing{..}) f (FieldType{..}) x y as =
    let start     = add1Minute (lTime x)
        end       = subtract1Minute (lTime y)
        dayBefore = addDays (-1)
        dayAfter  = addDays 1
        -- get values for same time period of day before and after
        valBefore = completeTimeSlice lTime f (dayBefore start) (dayBefore end) as
        valAfter  = completeTimeSlice lTime f (dayAfter start) (dayAfter end) as
        -- measure difference between the endpoints of the gap to see which is a better fit
        g v w     = abs <$> liftA2 (subtract `on` getValue) (v ^. f) (view f =<< w)
        m ss      = liftA2 (+) (g x $ NE.head <$> ss) (g y $ NE.last <$> ss)
        mBefore   = m valBefore
        mAfter    = m valAfter
        -- shift times to match current day
        nowBefore = dayOffset   1  pr <$> toList <$> valBefore
        nowAfter  = dayOffset (-1) pr <$> toList <$> valAfter
    in case liftA2 (>) mBefore mAfter of
        -- if data from day after is closer to gap at edges then prefer it
        Just True -> nowAfter  <|> nowBefore
        _         -> nowBefore <|> nowAfter


-- |Optionally get a time slice of data between given times
--  for which the specified field is complete
completeTimeSlice :: (a -> LocalTime)
                  -> Lens' a (Maybe b)
                  -> LocalTime
                  -> LocalTime
                  -> [a]
                  -> Maybe (NonEmpty a)
completeTimeSlice _ _ _ _ [] = Nothing
completeTimeSlice lTime f start end (a:as)
    -- skip until we reach the start time
    | lTime a < start  = completeTimeSlice lTime f start end as
    -- if at start time and value exists
    | lTime a == start && isJust (a ^. f)
                       = if start == end
                             then Just (a:|[])
                             else (a<|) <$> completeTimeSlice lTime f (add1Minute start) end as
    | otherwise        = Nothing


-- Functions for modifying time values
addDiffLocal :: NominalDiffTime -> LocalTime -> LocalTime
addDiffLocal dt t = utcToLocalTime utc $ addUTCTime dt (localTimeToUTC utc t)

addMinutes :: Int -> LocalTime -> LocalTime
addMinutes = addDiffLocal . realToFrac . (60*)

add1Minute :: LocalTime -> LocalTime
add1Minute = addMinutes 1

subtract1Minute :: LocalTime -> LocalTime
subtract1Minute = addMinutes (-1)

addDays :: Int -> LocalTime -> LocalTime
addDays = addDiffLocal . realToFrac . (86400*)

dayOffset :: Int -> Processing a -> [a] -> [a]
dayOffset n (Processing{..}) = map (\a -> setLTime a (addDays n $ lTime a))

isMediumGap :: Int -> Bool
isMediumGap m = m >= 300 && m <= 1440

