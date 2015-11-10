{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Tmy.OneMinSolar.FillAdjacent where

import Control.Lens                         (Lens', (^.), (.~), (&))
import Control.Applicative                  ((<$>),(<|>))
import Data.Maybe                           (isJust)
import Data.Time.Clock                      (addUTCTime,NominalDiffTime)
import Data.Time.LocalTime                  (LocalTime,localTimeToUTC,utcToLocalTime,utc)
import Data.List                            (intercalate)
import Data.List.NonEmpty                   (NonEmpty(..),(<|),toList)
import qualified Data.List.NonEmpty as NE   (head,last)

import Tmy.OneMinSolar.Functions
import Tmy.OneMinSolar.Types


fillAdjacent :: Processing a
       -> (Lens' a (Maybe b))
       -> FieldType b
       -> [a]
       -> [a]
fillAdjacent pr@(Processing{..}) f _ as = go as where 
    go (x:xs) = case x ^. f of
        Nothing -> x : go xs
        Just _  -> case minutesUntil pr f (lTime x) xs of
            Nothing -> x : xs
            Just (mins,y) -> if isMediumGap mins
                then let start = addDiffLocal 60 (lTime x)
                         end   = addDiffLocal (-60) (lTime y)
                         adj   = getAdjacentDaysValues pr f start end as
                     in case adj of 
                        Nothing -> x : go xs
                        Just vs -> x : go (update vs xs)
                else x : go xs
    go [] = []      

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
getAdjacentDaysValues :: Processing a
                      -> Lens' a (Maybe b)
                      -> LocalTime
                      -> LocalTime
                      -> [a]
                      -> Maybe [a]
getAdjacentDaysValues pr@(Processing{..}) f start end a =
    let dayBefore = addDay (-1)
        dayAfter  = addDay 1
        valBefore = toList <$> completeTimeSlice pr f (dayBefore start) (dayBefore end) a
        valAfter  = toList <$> completeTimeSlice pr f (dayAfter start) (dayAfter end) a
        nowBefore = dayOffset 1 pr <$> valBefore
        nowAfter  = dayOffset (-1) pr <$> valAfter
    in nowAfter <|> nowBefore
    

-- |Optionally get a time slice of data between given times
--  for which the specified field is complete
completeTimeSlice :: Processing a
                  -> Lens' a (Maybe b)
                  -> LocalTime
                  -> LocalTime
                  -> [a]
                  -> Maybe (NonEmpty a)
completeTimeSlice _ _ _ _ [] = Nothing
completeTimeSlice pr@(Processing{..}) f start end (a:as) 
    | lTime a < start  = completeTimeSlice pr f start end as
    -- | lTime a == start = errorTimes "0" [lTime a, start, end] --delete
    | lTime a == start && isJust (a ^. f)
                         = if lTime a == end
                             then Just (a:|[]) -- errorTimes "1" [lTime a, start, end] -- 
                             else (a<|) <$> completeTimeSlice pr f (addMinute start) end as --errorTimes "2" [lTime a, start, end] -- 
    | otherwise          = Nothing --errorTimes "4" [lTime a, start, end] -- 

errorTimes :: Show a => [Char] -> [a] -> b
errorTimes s l = error $ intercalate " | " $ s : (show <$> l)

-- Functions for modifying time values
addDiffLocal :: NominalDiffTime -> LocalTime -> LocalTime
addDiffLocal dt t = utcToLocalTime utc $ addUTCTime dt (localTimeToUTC utc t)

addMinute :: LocalTime -> LocalTime
addMinute = addDiffLocal 60

addDay :: Int -> LocalTime -> LocalTime
addDay = addDiffLocal . realToFrac . (86400*)

dayOffset :: Int -> Processing a -> [a] -> [a]
dayOffset n (Processing{..}) = map (\a -> setLTime a (addDay n $ lTime a)) 

isMediumGap :: Int -> Bool
isMediumGap m = m > 300 && m <= 1440
