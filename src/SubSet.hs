module SubSet where

import qualified AffineArithmetic as AA
import qualified IntervalArithmetic as IA
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L

-- 'Uset' is the 'universal set'
-- an empty set would be 'Total 0' or 'S.empty'
data Subset a b = Continuous (IA.Interval a) | Discrete (S.Set b) | Total Int | Uset deriving(Eq,Show,Ord)

-- Gives the narrowest interval that contains both given intervals
union :: (Ord a, Ord b, Show a, Show b) => Subset a b -> Subset a b -> Subset a b
union (Total x) (Total y) = Total $ max x y
union (Discrete x) (Discrete y) = Discrete $ S.union x y
union (Discrete x) Uset         = Uset
union (Discrete x) (Total n)    = if n == 0 then Discrete x
                                  else Total $ max n (S.size x)

union (Total n) (Discrete x) = union (Discrete x) (Total n)
union Uset      (Discrete x) = union (Discrete x) Uset

union Uset (Total n)         = Uset
union (Total n) Uset         = Uset

union Uset      Uset         = Uset
union (Continuous x) (Continuous y)  = Continuous $ IA.union x y
union (Continuous x) Uset            = Uset
union xx@(Continuous x) yy@(Total n) = if n == 0 then Continuous x
                                       else error $ "cannot mix discrete and continuous types " ++ show xx ++ " and " ++ show yy
union (Total n) (Continuous x) = union (Continuous x) (Total n)

union x y = error $ "cannot mix discrete and continuous types " ++ show x ++ " and " ++ show y

intersect :: (Ord a, Ord b, Show a, Show b) =>  Subset a b -> Subset a b -> Subset a b
intersect (Total x) (Total y) = Total (min x y)
intersect (Discrete x) (Discrete y) = Discrete $ S.intersection x y

intersect (Discrete x) Uset      = Discrete x
intersect (Discrete x) (Total n) = Total $ min n (S.size x)

intersect Uset (Discrete x)      = intersect (Discrete x) Uset
intersect (Total n) (Discrete x) = intersect (Discrete x) (Total n)

intersect Uset (Total n)         = Total n
intersect (Total n) Uset         = Total n

intersect Uset      Uset         = Uset

intersect (Continuous  x) (Continuous y) = Continuous $ IA.intersect x y
intersect (Continuous x) Uset            = Continuous x
intersect xx@(Continuous x) yy@(Total n) = if n == 0 then Continuous $ IA.Empty
                                           else error $ "cannot mix discrete and continuous types " ++ show xx ++ " and " ++ show yy
intersect (Total n) (Continuous x) = intersect (Continuous x) (Total n)
intersect x y = error $ "cannot mix discrete and continuous types " ++ show x ++ " and " ++ show y

joinIntervalList :: (Ord a, Ord b, Show a, Show b) =>  [Subset a b] -> Subset a b
joinIntervalList [x]    = x
joinIntervalList (x:xt) = union x $ joinIntervalList xt

createSet :: (Ord b) => [b] -> Subset a b
createSet xs = Discrete (S.fromList xs)

createInterval :: a -> a -> Subset a b
createInterval x y = Continuous (IA.Interval x y)

getSetData :: Subset a b -> [b]
getSetData (Discrete xs) = S.toList xs

high :: (Show a, Show b) => Subset a b -> Maybe a
high (Continuous x) = IA.high x
high x        = error $ "cannot define maximum for a discrete set " ++ show x

low :: (Show a, Show b) => Subset a b -> Maybe a
low (Continuous x) = IA.low x
low x        = error $ "cannot define minimum for a discrete set " ++ show x

isEmpty :: Eq a => Subset a b -> Bool
isEmpty (Continuous x) = (x == IA.Empty)
isEmpty (Discrete x) = S.null x
isEmpty Uset = False
isEmpty (Total n) = n == 0

fromNameAndInterval :: (Fractional a, Show a, Show b) => String -> Subset a b -> AA.AF a
fromNameAndInterval s (Continuous x) = AA.fromNameAndInterval s x
fromNameAndInterval s x = error $ "cannot apply affine arithmetic to discrete variable " ++ s ++ " of type " ++ show x

toInterval :: (Fractional a) => AA.AF a -> Subset a b
toInterval x = Continuous (AA.toInterval x)

getOnlyNumericRanges :: M.Map String (Subset a b) -> M.Map String (IA.Interval a)
getOnlyNumericRanges ms = M.map (\(Continuous x) -> x) $ M.filter (\x -> case x of {Continuous _ -> True; _ -> False}) ms

