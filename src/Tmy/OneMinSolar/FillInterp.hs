{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}


module Tmy.OneMinSolar.FillInterp where

import Control.Lens                         (Lens', (^.), (.~), (&), (+~))
import Data.Maybe                           (fromJust)
import Data.Time.Lens                       (flexDT, minutes)

import Tmy.OneMinSolar.Functions
import Tmy.OneMinSolar.Types

fillInterp :: Processing a
       -> (Lens' a (Maybe b))
       -> FieldType b
       -> [a]
       -> [a]
fillInterp pr@(Processing{..}) f ft as@(a:xs) =
    -- check that we have a record with a value for this field
    case a ^. f of
        -- if not then we must be at the start of the list, so iterate until we find one
        Nothing -> a : fillInterp pr f ft xs
        Just _  ->
            -- if we do then check how long until the next value for this field
            case minutesUntil pr f (lTime a) xs of
                -- if not then we must be at the end of the list
                Nothing -> as
                Just ((mins, b)) ->
                    if mins > 1 && isLessThan5Hours mins
                        then let xs' = linearlyInterpolate pr f ft mins a b xs
                             in  a : fillInterp pr f ft xs'
                        else a : fillInterp pr f ft xs
fillInterp _ _ _ [] = []


-- | Check that the infilling of values has succeeded and there are no more gaps
--   of data shorter than the infill max gap length.
check :: (Show a, Show b)
      => Processing a
      -> (Lens' a (Maybe b))
      -> FieldType b
      -> [a]
      -> [a]
check pr@(Processing{..}) f _ ss = go ss where
    go as@(a:xs) =
        -- check if a has a value for this time
        case a ^. f of
            Nothing -> a : go xs -- skip until we find a value for the field
            Just _  ->
                case minutesUntil pr f (lTime a) xs of
                    Nothing -> as
                    Just ((mins, b)) ->
                        if mins > 1 && isLessThan5Hours mins
                            then error ("Found a gap of " ++ show mins
                                        ++ " minutes, shorter than the minimum 300. From "
                                        ++ show (lTime a) ++ " to " ++ show (lTime b)
                                        ++ ".\n\nThe two records are:\n\n"++ show a ++ "\n\n" ++ show b
                                        ++ "\n\na: " ++ show (a ^. f) ++ "\nb: " ++ show (b ^. f))
                            else a : go xs
    go [] = []


isLessThan5Hours :: Int -> Bool
isLessThan5Hours mins = mins < 300


linearlyInterpolate :: Processing a
                    -> (Lens' a (Maybe b))
                    -> FieldType b
                    -> Int
                    -> a
                    -> a
                    -> [a]
                    -> [a]
linearlyInterpolate _ _ _ 0 _ _ xs = xs
linearlyInterpolate (Processing{..}) f (FieldType{..}) num a b xs' = go 1 xs' where
    lt x       = lTime x                        -- get the LocalTime from an AwStats
    addMin x m = lt x & flexDT.minutes +~ m     -- add minutes to a LocalTime
    va         = getValue (fromJust (a ^. f))   -- the mean value of the field for a
    vb         = getValue (fromJust (b ^. f))   -- the mean value of the field for b
    vincr      = (vb - va) / fromIntegral (num) -- the linear increment
    val n      = va + (vincr * fromIntegral n)  -- the new mean of the nth linearly interpolated record
    go _ []    = []
    go n ss@(x:xs)
        -- we've done as many infills as we needed, all done
        | n >= num           = ss
        -- found a record with the right time, modify with new stat
        | lt x == addMin a n = (x & f .~ Just (mkValue (val n))) : go (n+1) xs
        -- no record with the right time, make one and set the stat
        | otherwise          = (mkEmpty (stNum a) (addMin a n) & f .~ Just (mkValue (val n))) : go (n+           1)         ss


fillInterpAndCheck :: (Show a, Show b)
      => Processing a
      -> (Lens' a (Maybe b))
      -> FieldType b
      -> [a]
      -> [a]
fillInterpAndCheck =
  composeProcessors fillInterp check
