{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Tmy.OneMinSolar.FillAdjacent where

import Control.Lens                         (Lens', (^.), (.~), (&))
import Control.Applicative                  ((<$>),(<|>))
import Data.Maybe                           (isJust,fromMaybe)
import Data.Function                        (on)
import Data.Time.Clock                      (addUTCTime,NominalDiffTime)
import Data.Time.LocalTime                  (LocalTime,localTimeToUTC,utcToLocalTime,utc)
import Data.List                            (intercalate)
import Data.List.NonEmpty                   (NonEmpty(..),(<|),toList)
import qualified Data.List.NonEmpty as NE   (head,last)

import Tmy.OneMinSolar.Functions
import Tmy.OneMinSolar.Types

import GHC.Stack


fillAdjacent :: Processing a
       -> (Lens' a (Maybe b))
       -> FieldType b
       -> [a]
       -> [a]
fillAdjacent pr@(Processing{..}) f ft as@(a:xs) =
    -- check that we have a record with a value for this field
    case a ^. f of
        -- if not then we must be at the start of the list, so iterate until we find one
        Nothing -> a : fillAdjacent pr f ft xs
        Just _  ->
            -- if we do then check how long until the next value for this field
            case minutesUntil pr f (lTime a) xs of
                -- if not then we must be at the end of the list
                Nothing -> as
                Just ((mins, b)) ->
                    if isMediumGap mins
                        then let start = lTime a
                                 end   = addDiffLocal (realToFrac mins) start --lTime b
                                 g     = getAdjacentDaysValues pr f start end as
                             in case g of 
                                Nothing -> a : fillAdjacent pr f ft xs
                                Just h  -> let xs' = update pr f ft h xs    
                                           in  a : fillAdjacent pr f ft xs'
                        else a : fillAdjacent pr f ft xs
fillAdjacent _ _ _ [] = []


update :: Processing a -> (Lens' a (Maybe b)) -> FieldType b -> [a] -> [a] -> [a]
update pr@(Processing{..}) f ft (x:xs) (a:as)  
    -- if time matches update field with value
    | lTime a == lTime x = (a & f .~ (x ^. f)) : update pr f ft xs as
    -- if time is missing add new field with value
    | otherwise          = (mkEmpty (stNum x) (lTime x) & f .~ (x ^. f)) : update pr f ft xs (a:as)
update _ _ _ _ _ = []
    


{--
fillAdjacent' :: Processing a
       -> Lens' a (Maybe b)
       -> FieldType b
       -> [a]
       -> [a]
fillAdjacent' _ _ _ [] = []
fillAdjacent' pr f _ a =
    unChunk $ map (replaceEmptyChunk pr a) (chunkByField f a)
replaceEmptyChunk :: Processing a
                  -> [a]
                  -> Chunk a b 
                  -> Chunk a b
replaceEmptyChunk pr as c@(Chunk False f a) 
    | isMediumGap $ chunkMinT pr c =
        let start = chunkStartT pr c
            end   = chunkEndT pr c
            g     = getAdjacentDaysValues pr f start end
        --in if isJust (g as) then (error "yes") else (error "no") --errorTimes [start,end] 
        in c { list = fromMaybe a (g as) }
    | otherwise   = error $ "not medium: " ++ show (chunkMinT pr c) ++ " | " ++ show (chunkStartT pr c) ++ " | " ++ show (chunkEndT pr c) --c
replaceEmptyChunk _ _ c = c --error $ "not gap: " ++ show (chunkMinT pr c) ++ " | " ++ show (chunkStartT pr c) ++ " | " ++ show (chunkEndT pr c) --c
--}


-- |Optionally get values between the given timestamps shifted by a day
--  and for which all entries of the specified field exist
getAdjacentDaysValues :: Processing a
                      -> Lens' a (Maybe b)
                      -> LocalTime
                      -> LocalTime
                      -> [a]
                      -> Maybe [a]
getAdjacentDaysValues pr f start end a =
    let dayBefore = addDay (-1)
        dayAfter  = addDay 1
        valBefore = toList <$> completeTimeSlice pr f (dayBefore start) (dayBefore end) a
        valAfter  = toList <$> completeTimeSlice pr f (dayAfter start) (dayAfter end) a
        nowBefore = dayOffset 1 pr <$> valBefore
        nowAfter  = dayOffset (-1) pr <$> valAfter
    in nowAfter <|> nowBefore
    --in errorTimes "d" [start, dayAfter start, dayBefore start, end, dayAfter end, dayBefore end]

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
                             else (a<|) <$> completeTimeSlice pr f (addMinute start) end as -- errorTimes "2" [lTime a, start, end] 
    | otherwise          = Nothing -- errorTimes "4" [lTime a, start, end] --

errorTimes s l = error $ intercalate " | " $ s : (show <$> l)

addDiffLocal :: NominalDiffTime -> LocalTime -> LocalTime
addDiffLocal dt t = utcToLocalTime utc $ addUTCTime dt (localTimeToUTC utc t)

addMinute :: LocalTime -> LocalTime
addMinute = addDiffLocal 60

addDay :: Int -> LocalTime -> LocalTime
addDay = addDiffLocal . realToFrac . (86400*)

dayOffset :: Int -> Processing a -> [a] -> [a]
dayOffset n (Processing{..}) = map (\a -> setLTime a (addDay n $ lTime a)) 

isMediumGap :: Int -> Bool
--isMediumGap m = m > 0 && m <= 1440
isMediumGap m = m > 0 && m <= 1440

--Chunks
{--
data Chunk a b = Chunk { bool :: Bool
                       , lens :: Lens' a (Maybe b)
                       , list :: NonEmpty a
                       }

chunkByField :: Lens' a (Maybe b) -> [a] -> [Chunk a b]
chunkByField f as =
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
    minDiff (chunkEndT pr c) (chunkStartT pr c)

unChunk :: [Chunk a b] -> [a]
unChunk = let f (Chunk _ _ a) as = toList a ++ as
           in foldr f []
--}

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
{--
gatherBy :: (Eq b) => (a -> b) -> [a] -> [NonEmpty a]
gatherBy _ [] = []
gatherBy f as@(a:_) =
  let p         = ((==) `on` f) a 
      (x:xs,ys) = span p as
  in (x:|xs) : gatherBy f ys

-- |Gather equal and adjacent elements together.
gather :: (Eq a) => [a] -> [NonEmpty a]
gather = gatherBy id
--}
