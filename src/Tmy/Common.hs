{-# LANGUAGE OverloadedStrings #-}

module Tmy.Common where


import Data.ByteString                      (ByteString, empty)
import Data.Csv
import Data.Maybe                           (fromMaybe)
import Data.Semigroup                       (Semigroup, Min(..), Max(..), (<>))
import Data.Text                            (Text, append)
import Data.Text.Encoding                   (encodeUtf8)
import Data.Time.LocalTime                  (LocalTime(..), TimeOfDay(..), localTimeOfDay, localDay, todHour, todMin)

import Tmy.Csv


data Stat a = Stat
    { stSum       :: !a
    , stMax       :: !(Max a)
    , stMin       :: !(Min a)
    , stCount     :: !Int
    , stFillCount :: !Int
    } deriving (Show, Eq, Ord)

instance (Num a, Ord a) => Semigroup (Stat a) where
    (Stat amean amax amin acnt afcnt) <> (Stat bmean bmax bmin bcnt bfcnt) =
        Stat (amean +  bmean)
             (amax  <> bmax)
             (amin  <> bmin)
             (acnt  +  bcnt)
             (afcnt +  bfcnt)


data Mean a = Mean
    { sSum       :: !a
    , sCount     :: !Int
    , sFillCount :: !Int
    } deriving (Show, Eq, Ord)

instance (Num a, Ord a) => Semigroup (Mean a) where
    (Mean asum acount afcount) <> (Mean bsum bcount bfcount) =
        Mean (asum + bsum) (acount + bcount) (afcount + bfcount)


statRecord :: (ToField a, Fractional a, Show a) => ByteString -> Maybe (Stat a) -> NamedRecord
statRecord prefix Nothing =
    let col = (prefix <>)
    in  namedRecord
        [ col " mean"       .= empty
        , col " max"        .= empty
        , col " min"        .= empty
        , col " count"      .= empty
        , col " fill count" .= empty
        ]
statRecord prefix (Just s@(Stat _ (Max smax) (Min smin) scount sfcount)) =
    let col = (prefix <>)
    in  namedRecord
        [ col " mean"       .= statMean s
        , col " max"        .= smax
        , col " min"        .= smin
        , col " count"      .= scount
        , col " fill count" .= sfcount
        ]


statMean :: Fractional a => Stat a -> a
statMean (Stat ssum _ _ scount sfcount) = (ssum / fromIntegral (scount + sfcount))


meanRecord :: (ToField a, Fractional a, Show a) => Text -> Maybe (Mean a) -> NamedRecord
meanRecord prefix Nothing =
    let col = encodeUtf8 . append prefix
    in  namedRecord
        [ col " mean"       .= empty
        , col " count"      .= empty
        , col " fill count" .= empty
        ]
meanRecord prefix (Just (Mean ssum scount sfcount)) =
    let col = encodeUtf8 . append prefix
    in  namedRecord
        [ col " mean"       .= (ssum / fromIntegral (scount + sfcount))
        , col " count"      .= scount
        , col " fill count" .= sfcount
        ]


mMean :: Fractional a => Mean a -> a
mMean (Mean ssum scount sfcount) = ssum / fromIntegral (scount + sfcount)


maybeStat :: (a -> Spaced (Maybe b))
          -> (a -> Spaced (Maybe b))
          -> (a -> Spaced (Maybe b))
          -> a
          -> Maybe (Stat b)
maybeStat meanF maxF minF a =
    case maybeMean of
        Just mean -> Just (mkStat mean (fromMaybe mean maybeMax) (fromMaybe mean maybeMin))
        Nothing   -> Nothing
    where
        maybeMean = unSpaced (meanF a)
        maybeMax  = unSpaced (maxF a)
        maybeMin  = unSpaced (minF a)


maybeQualStat :: (a -> Spaced Char)
              -> (a -> Spaced (Maybe b))
              -> (a -> Spaced (Maybe b))
              -> (a -> Spaced (Maybe b))
              -> a
              -> Maybe (Stat b)
maybeQualStat meanQf meanF maxF minF a =
    case qFilter meanQf meanF a of
        Just _  -> maybeStat meanF maxF minF a
        Nothing -> Nothing


qFilter :: (a -> Spaced Char)
        -> (a -> Spaced (Maybe b))
        -> a
        -> Maybe b
qFilter qf vf a =
    if unSpaced (qf a) `elem` "YNSF"
        then unSpaced (vf a)
        else Nothing


mkStat :: a -> a -> a -> Stat a
mkStat smean smax smin = Stat smean (Max smax) (Min smin) 1 0


mkFillStat :: a -> a -> a -> Stat a
mkFillStat smean smax smin = Stat smean (Max smax) (Min smin) 0 1


mkMean :: a -> Mean a
mkMean a = Mean a 1 0


mkFillMean :: a -> Mean a
mkFillMean a = Mean a 0 1


hourGrouper :: (a -> LTime) -> a -> a -> Bool
hourGrouper f a b = floorMinute (f a) == floorMinute (f b)


floorMinute :: LTime -> LTime
floorMinute (LTime a) = LTime (LocalTime (localDay a) (TimeOfDay (todHour (localTimeOfDay a)) 0 0))


minute :: LTime -> Int
minute (LTime a) = (todMin . localTimeOfDay) a


mergeWith :: Ord c => (a -> c)
                   -> (b -> c)
                   -> (Maybe a -> Maybe b -> r)
                   -> [a] -> [b] -> [r]
mergeWith fa fb comb xs ys = go xs ys where
    go [] [] = []
    go [] bs = map (comb Nothing . Just)      bs
    go as [] = map (flip comb Nothing . Just) as
    go aas@(a:as) bbs@(b:bs) = case compare (fa a) (fb b) of
        LT -> comb (Just a) Nothing  : go as  bbs
        EQ -> comb (Just a) (Just b) : go as  bs
        GT -> comb Nothing  (Just b) : go aas bs


combine :: Semigroup b => (a -> b) -> a -> a -> b
combine f a b = f a <> f b

