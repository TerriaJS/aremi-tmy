{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Tmy.OneMinSolar.FillAdjacent where

import Control.Lens                         (Lens', (^.))
import Control.Applicative                  ((<$>),(<|>))
import Data.Maybe                           (isJust,isNothing)
import Data.Function                        (on)
import Data.Time.Clock                      (addUTCTime,NominalDiffTime)
import Data.Time.LocalTime                  (LocalTime,localTimeToUTC,utcToLocalTime,utc)
import Data.List.NonEmpty                   (NonEmpty(..))
import qualified Data.List.NonEmpty as NE   (head,last)

import Tmy.OneMinSolar.Functions
import Tmy.OneMinSolar.Types

fillAdjacent :: Processing a
       -> (Lens' a (Maybe b))
       -> FieldType b
       -> [a]
       -> [a]
fillAdjacent pr@(Processing{..}) f ft as@(a:xs) = 
  case a ^. f of
    Nothing -> a : fillAdjacent pr f ft xs
    Just _  ->
      case minutesUntil pr f (lTime a) xs of
        Nothing         -> as
        Just (mins, b)  ->
          if isMediumGap mins
            then case getAdjacentDaysValues pr f ft (lTime a) (lTime b) xs of
              Nothing  -> a : fillAdjacent pr f ft xs
              Just xs' -> a : xs' ++ fillAdjacent pr f ft xs              
            else a : fillAdjacent pr f ft xs
fillAdjacent _ _ _ [] = []

replaceEmptyChunk :: Chunk a b -> Chunk a b
replaceEmptyChunk c@(Chunk False _ _) = c
replaceEmptyChunk c@(Chunk True f a) = 



-- |Optionally get values between the given timestamps shifted by a day
--  and for which all entries of the specified field exist
getAdjacentDaysValues :: Processing a
                      -> (Lens' a (Maybe b))
                      -> FieldType b
                      -> LocalTime
                      -> LocalTime
                      -> [a]
                      -> Maybe [a]
getAdjacentDaysValues pr f ft start end a =
    let dayBefore = addDay (-1)
        dayAfter  = addDay 1
        yesterday = completeTimeSlice pr f ft (dayBefore start) (dayBefore end) a
        tomorrow  = completeTimeSlice pr f ft (dayAfter start) (dayAfter end) a
    in yesterday <|> tomorrow


-- |Optionally get a timeslice of data between given times 
--  for which the specified field is complete
completeTimeSlice :: Processing a
                  -> (Lens' a (Maybe b))
                  -> FieldType b
                  -> LocalTime
                  -> LocalTime
                  -> [a]
                  -> Maybe [a]
completeTimeSlice _ _ _ _ _ [] = Nothing
completeTimeSlice pr@(Processing{..}) f ft start end (a:as) 
    | (lTime a) > start  = completeTimeSlice pr f ft start end as
    | (lTime a) == start && isJust (a ^. f) 
                         = if (lTime a) == end
                           then Just [a]
                           else (a:) <$> completeTimeSlice pr f ft (addMinute start) end as
    | otherwise          = Nothing

addDiffLocal :: NominalDiffTime -> LocalTime -> LocalTime
addDiffLocal dt t = utcToLocalTime utc $ addUTCTime dt (localTimeToUTC utc t)

addMinute :: LocalTime -> LocalTime
addMinute = addDiffLocal 60

addDay :: Int -> LocalTime -> LocalTime
addDay = addDiffLocal . realToFrac . (1440*)

isMediumGap :: Int -> Bool
isMediumGap mins = mins >= 300 && mins <= 1440 

--Chunks
data Chunk a b = Chunk Bool (Lens' a (Maybe b)) (NonEmpty a)

chunkByField' :: [a] -> (Lens' a (Maybe b)) -> [Chunk a b]
chunkByField' as f =
    let p = (isJust . (^. f)) 
        g xs = Chunk (p (NE.head xs)) f xs  
    in fmap g (gatherBy p as)
    
chunkStartT :: Processing a -> Chunk a b -> LocalTime
chunkStartT (Processing{..}) (Chunk _ _ (a:|_)) 
    = lTime a

chunkEndT:: Processing a -> Chunk a b -> LocalTime
chunkEndT (Processing{..}) (Chunk _ _ as) 
    = lTime (NE.last as)

chunkMinT :: Processing a -> Chunk a b -> Int
chunkMinT pr c = 
    minDiff (chunkStartT pr c) (chunkEndT pr c)

-- |Gather adjacent elements of a list together for which the given function 
--  evaluates to the same value. Returns a list of non-empty lists.
--
-- >>> gatherBy (>3) [1,2,3,4,5,4,3,2,1]
-- [1 :| [2,3],4 :| [5,4],3 :| [2,1]]
--
-- >>> gatherBy isNothing [Nothing, Nothing, Just 1, Just 2, Just 3]
-- [Nothing :| [Nothing],Just 1 :| [Just 2,Just 3]]
--
-- >>> gatherBy id []
-- []
--
gatherBy :: (Eq b) => (a -> b) -> [a] -> [NonEmpty a]
gatherBy _ [] = []
gatherBy f as@(a:_) =
  let p         = ((==) `on` f) a 
      (x:xs,ys) = span p as
  in (x:|xs) : gatherBy f ys

-- |Gather equal and adjacent elements together.
gather :: (Eq a) => [a] -> [NonEmpty a]
gather = gatherBy id
