{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AffineArithmetic where

import IntervalArithmetic hiding (Empty)
import Data.Map as M
import Data.Maybe

-- TODO adjust values for floating point rounding errors

-- We are using affine form x_0 [x_1..x_n] x_(n+1)
data AF a = AF a (M.Map String a) a
          | Empty
          deriving (Show)

-- Define operations and conversions
instance (Eq a, Num a) => Eq (AF a) where
    a@(AF x xs xe) == b@(AF y ys ye) = x == y && and [lookupVar a k == lookupVar b k | k <- keys xs ++ keys ys] && xe == 0 && ye == 0
    Empty == Empty = True
    _ == _ = False

instance (Ord a, Fractional a) => Num (AF a) where
    negate        = neg
    (+)           = add
    (*)           = mul
    fromInteger x = AF (fromInteger x) M.empty 0
    abs           = afAbs
    signum        = afSignum

instance (Ord a, Num a) => Ord (AF a) where
    a < b  = strictlyWithin (toInterval a) (toInterval b)
    a > b  = b < a
    a <= b = within (toInterval a) (toInterval b)
    a >= b = b <= a

instance (Ord a, Floating a) => Fractional (AF a) where
    a / b          = a * inv b
    fromRational x = AF (fromRational x) M.empty 0

instance (Ord a, Floating a) => Floating (AF a) where
    pi  = AF pi M.empty 0
    exp = undefined
    log = afLog
    sin = undefined
    cos = undefined
    asin = undefined
    acos = undefined
    atan = undefined
    sinh = undefined
    cosh = undefined
    asinh = undefined
    acosh = undefined
    atanh = undefined

(**) :: (Ord a, Floating a) => AF a -> AF a -> AF a
(**) = pow
infixr 8 **

lookupVar :: (Num a) => AF a -> String -> a
lookupVar (AF _ xs _) n = findWithDefault 0 n xs

-- All the operations assume that AF as have the same number of terms! (longer AF a get)
add :: Num a => AF a -> AF a -> AF a
add (AF x xs xe) (AF y ys ye) = AF (x+y) (M.unionWith (+) xs ys) (xe+ye)
add _ Empty = Empty
add x y = add y x

neg :: Num a => AF a -> AF a
neg (AF x xs xe) = AF (-x) (M.map negate xs) xe
neg Empty = Empty

mul :: Num a => AF a -> AF a -> AF a
mul (AF x xs xe) (AF y ys ye) = AF (x*y) (unionWith (+) (M.map (x*) ys) (M.map (y*) xs)) e
    where e = abs x * ye + abs y * xe +
              sum (ye : [abs a | a <- elems xs]) *
              sum (xe : [abs b | b <- elems ys])
mul Empty _ = Empty
mul x y = mul y x

inv :: (Ord a, Fractional a) => AF a -> AF a
inv af
  | ax > 0 || bx < 0 = affineApprox [(af, alpha)] (zeta * signum ax) delta
  | otherwise        = mapToIntervalAsAF af infInterval
    where (Interval ax bx) = toInterval af
          edges = [abs ax, abs bx]
          (Interval a b) = Interval (minimum edges) (maximum edges)
          alpha = -1/b ^ 2
          res = Interval (1/b-alpha*b) (1/a-alpha*a)
          zeta  = intervalCenter res
          delta = intervalRange res

-- TODO Come up with something more efficient, a or AF a instead of Integer
pow :: (Ord a, Floating a) => AF a -> AF a -> AF a
pow af (AF n m 0)
  | not $ M.null m     = error "POW only supported with constant exponents"
  | ax >= 0 || bx <= 0 = let alpha = n*ax Prelude.** (n-1)
                             res = Interval (ax Prelude.** n - alpha*ax) (bx Prelude.** n - alpha*bx)
                             zeta = intervalCenter res
                             delta = intervalRange res
                         in affineApprox [(af, alpha)] zeta delta
  | otherwise              = error "POW base can not contain 0 (not yet implemented)"
      where (Interval ax bx) = toInterval af

afLog :: (Ord a, Floating a) => AF a -> AF a
afLog af = affineApprox [(af, alpha)] zeta delta
    where (Interval ax bx) = toInterval af
          alpha = 1/bx
          res = Interval (log ax - alpha*ax) (log bx - alpha*bx)
          zeta  = intervalCenter res
          delta = intervalRange res

afAbs :: (Ord a, Fractional a) => AF a -> AF a
afAbs af
  | a >= 0    = af
  | b <= 0    = -af
  | otherwise = affineApprox [(af, 0)] r r
  where x@(Interval a b) = toInterval af
        r = intervalRange x

afSignum :: AF a -> AF a
afSignum = error "Signum function not yet supported"

-- Utilities --
-- Gives the max deviation from center of AF a
range :: (Num a) => AF a -> a
range (AF _ xs xe) = sum [abs x | x <- elems xs] + abs xe

fixEpsilon :: (Ord a, Num a) => String -> a -> AF a -> AF a
fixEpsilon name val (AF x xs xe)
  | val >= -1 && val <= 1 = AF (x+(fromMaybe 0 $ M.lookup name xs)*val) (M.delete name xs) xe
  | otherwise = error "Epsilon is not within range [-1, 1]"

fixEpsilonInterval :: (Ord a, Fractional a) => String -> Interval a -> AF a -> AF a
fixEpsilonInterval name val@(Interval a b) (AF x xs xe)
  | a >= -1 && b <= 1 = AF (x+alpha*c) (M.alter alterer name xs) xe
  | otherwise         = error "Interval must be within [-1, 1]"
  where alpha = fromMaybe 0 $ M.lookup name xs
        r = intervalRange val
        c = intervalCenter val
        alterer (Just x) = if r == 0 then Nothing else Just (r*x)
        alterer Nothing = Nothing

-- Convert AF a to Interval (loss of AF a information)
toInterval :: (Num a) => AF a -> Interval a
toInterval af@(AF c _ _) = Interval (c-r) (c+r)
    where r = range af

toIntervalWithoutError :: (Num a) => AF a -> Interval a
toIntervalWithoutError af@(AF c _ e) = Interval (c-r) (c+r)
    where r = (range af) - e

fromNameAndInterval :: (Fractional a) => String -> Interval a -> AF a
fromNameAndInterval n i = AF (intervalCenter i) (M.singleton n (intervalRange i)) 0

-- Maps the given AF a linearly to be equivalent to the given interval
mapToIntervalAsAF :: (Ord a, Fractional a) => AF a -> Interval a -> AF a
mapToIntervalAsAF a@(AF _ as ae) (Interval x y)
  | r > 0     = AF ((x+y)/2) (M.map (\v -> v/r*alpha) as) (ae/r*alpha)
  | otherwise = AF ((x+y)/2) M.empty 0
    where r = range a
          alpha = (y-x)/2

affineApprox :: (Ord a, Fractional a) => [(AF a, a)] -> a -> a -> AF a
affineApprox [(af, alpha)] zeta delta = addError ((AF alpha M.empty 0) * af + (AF zeta M.empty 0)) delta

divideAF :: (Ord a, Fractional a) => AF a -> Int -> [AF a]
divideAF af n = Prelude.map errf [mapToIntervalAsAF af i | i <- divideInterval (toInterval af) n]
    where errf x = if n == 1 
                       then addError x 0
                       else addError x (range x)

joinAFs :: (Ord a, Num a) => [AF a] -> Interval a
joinAFs [af]   = toInterval af
joinAFs (a:at) = IntervalArithmetic.union (toInterval a) (joinAFs at)

addError :: (Ord a, Num a) => AF a -> a -> AF a
addError (AF x xs xe) e = AF x xs (xe+e)

lerp :: (Fractional a) => (a,a) -> (a,a) -> a -> a
lerp (xa,ya) (xb,yb) x = ya+(x-xa)*(yb-ya)/(xb-xa)
