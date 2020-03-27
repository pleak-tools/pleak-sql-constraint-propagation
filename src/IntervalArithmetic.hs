module IntervalArithmetic where

import Text.Printf

data Interval a = Interval a a
                | Empty
    deriving (Eq)

instance (Show a) => Show (Interval a) where
    show (Interval a b) = "Interval " ++ (show a) ++ " " ++ (show b)
    show Empty = "empty"

instance Ord a => Ord (Interval a) where
    a < b  = strictlyWithin a b
    a > b  = b < a
    a <= b = within a b
    a >= b = b <= a

instance (Ord a, Num a) => Num (Interval a) where
    (Interval xa xb) + (Interval ya yb) = Interval (xa+ya) (xb+yb)
    (Interval xa xb) * (Interval ya yb) = Interval (minimum edges) (maximum edges)
        where edges = [xa*ya, xa*yb, xb*ya, xb*yb]
    abs = undefined
    signum = undefined
    negate (Interval xa xb) = Interval (-xb) (-xa)
    fromInteger x = Interval (fromInteger x) (fromInteger x)

infInterval :: Fractional a => Interval a
infInterval = Interval (-1/0) (1/0)

zeroInterval :: Num a => Interval a
zeroInterval = Interval 0 0

unitInterval :: Num a => Interval a
unitInterval = Interval 1 1

low :: Interval a -> Maybe a
low (Interval a _) = Just a
low _ = Nothing

high :: Interval a -> Maybe a
high (Interval _ a) = Just a
high _ = Nothing

-- Gives the narrowest interval that contains both given intervals
union :: Ord a => Interval a -> Interval a -> Interval a
union Empty Empty = Empty
union Empty i@(Interval _ _) = i
union i@(Interval _ _) Empty = i
union (Interval xa xb) (Interval ya yb) = Interval (minimum edges) (maximum edges)
    where edges = [xa,xb,ya,yb]

intersect :: Ord a => Interval a -> Interval a -> Interval a
intersect Empty _      = Empty
intersect _ Empty      = Empty
intersect (Interval xa xb) (Interval ya yb)
  | xb < ya || xa > yb = Empty
  | otherwise          = Interval (max ya xa) (min yb xb)

joinIntervalList :: Ord a => [Interval a] -> Interval a
joinIntervalList [x]    = x
joinIntervalList (x:xt) = union x $ joinIntervalList xt

intervalCenter :: Fractional a => Interval a -> a
intervalCenter (Interval a b) = (b+a)/2

intervalRange :: Fractional a => Interval a -> a
intervalRange (Interval a b) = (b-a)/2

within :: Ord a => Interval a -> Interval a -> Bool
within (Interval xa xb) (Interval ya yb) = ya <= xa && xb <= yb
within Empty _ = True
within _ Empty = False

strictlyWithin :: Ord a => Interval a -> Interval a -> Bool
strictlyWithin (Interval xa xb) (Interval ya yb) = ya < xa && xb < yb

inflate :: (Num a) => Interval a -> a -> Interval a
inflate (Interval a b) delta = Interval (a-abs a*delta - delta) (b+abs b*delta + delta)

-- Divides given integral into n subintervals
divideInterval :: (Num a, Fractional a) => Interval a -> Int -> [Interval a]
divideInterval (Interval a b) n = [Interval (a+(fromIntegral i - 1)*s) (a+(fromIntegral i)*s) | i <- [1..n]]
    where s = (b-a)/nf
          nf = fromIntegral n

relativeInterval :: (Ord a, Fractional a) => Interval a -> Interval a -> Interval a
relativeInterval (Interval xa xb) (Interval ya yb) = intersect (Interval (-1) 1) ((Interval ((xa-ya)/(yb-ya)) ((xb-ya)/(yb-ya))) * 2 - 1)
