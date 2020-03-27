module IntervalArray where

import AffineArithmetic
import IntervalArithmetic as IA

import qualified Data.Map as M
import qualified Data.List as L
import Data.Fixed (mod')
import Debug.Trace

-- each Dimension contains the name of the next variable and
-- the range that variable lies in
data MultiDimArray a b
  = Dimension String (Interval a) [MultiDimArray a b]
  | Elem b

instance (Show a, Show b) => Show (MultiDimArray a b) where
    show (Elem x) = "Elem " ++ show x
    show (Dimension n i a) = "Dimension " ++ n ++ " " ++ (show i) ++ " [" ++ (L.intercalate "," $ map show a) ++ "]"

instance Functor (MultiDimArray a) where
  fmap f (Dimension n i arr) = Dimension n i $ map (fmap f) arr
  fmap f (Elem x) = Elem $ f x

-- Creates an MultiDimArray from name/interval pairs
fromNamedIntervals :: (Fractional a) => M.Map String (Interval a) -> MultiDimArray a [M.Map String (Interval a)]
fromNamedIntervals x = fromNamedIntervals_ (M.toList x) [M.empty]

fromNamedIntervals_ :: (Fractional a) => [(String, (Interval a))] -> [M.Map String (Interval a)] -> MultiDimArray a [M.Map String (Interval a)]
fromNamedIntervals_ [] env = Elem env
fromNamedIntervals_ ((n,i):xt) env = Dimension n i [fromNamedIntervals_ xt newEnv]
    where newEnv = map (M.insert n i) env

-- Divides every dimension of the array into `n` subdivisions
divideMultiDimArray :: (Fractional a) => MultiDimArray a [M.Map String (Interval a)] -> Int -> MultiDimArray a [M.Map String (Interval a)]
divideMultiDimArray arr n
  | n == 1    = arr
  | n > 1     = divideMultiDimArray_ arr n $ map (const M.empty) [1..(dimensions arr)]
  | otherwise = error "Iteration count must be greater than 0"

divideMultiDimArray_ :: (Fractional a) => MultiDimArray a [M.Map String (Interval a)] -> Int -> [M.Map String (Interval a)] -> MultiDimArray a ([M.Map String (Interval a)])
divideMultiDimArray_ (Dimension name i arr) n env = (Dimension name i $ concatMap (\x -> [divideMultiDimArray_ x n (newEnv af) | af <- afDivs]) arr)
    where newEnv af = map (M.insert name af) env
          afDivs = divideInterval i n
divideMultiDimArray_ (Elem _) _ e = Elem e

dimensions :: MultiDimArray a b -> Int
dimensions (Dimension _ _ arr) = 1 + (dimensions $ head arr)
dimensions (Elem _) = 0

-- Gathers all results together and outputs the resulting interval and error factor
outputInterval :: (Ord a, Num a) => MultiDimArray a (AF a) -> Interval a
outputInterval (Dimension _ _ arr) = foldl union IA.Empty (map outputInterval arr)
outputInterval (Elem af) = toInterval af

-- Finds the maximum error between all the subdivisions in the array
maxError :: (Ord a, Num a) => MultiDimArray a (AF a) -> a
maxError (Dimension _ _ arr) = maximum (-1:map maxError arr)
maxError (Elem (AF _ _ e)) = e
maxError (Elem AffineArithmetic.Empty) = -1

-- Takes a list of variable name and known interval pairs and narrows the array to those values
fixVariableIntervals :: (Show a, RealFrac a) =>  M.Map String (Interval a) -> MultiDimArray a (AF a) -> MultiDimArray a (AF a)
fixVariableIntervals vars arr = foldl (\a (n, v) -> fixVariableInterval n v a) arr $ M.toList vars

-- Same as above but for only a single name/interval pair
fixVariableInterval :: (Show a, RealFrac a) => String -> Interval a -> MultiDimArray a (AF a) -> MultiDimArray a (AF a)
fixVariableInterval name v@(Interval x y) (Dimension n i@(Interval a b) arr)
  | n == name = Dimension n v $ map (uncurry $ fixVariableInterval_ name v) sub_arr_w_intervals
  | otherwise = Dimension n i $ map (fixVariableInterval name v) arr
  -- Calculate the relative interval in relation to the whole input range of the variable
  -- The value of 0 would correspond to the beginning of the input range and 1 would correspond to the end of the input range
  where xr = ((x-a)/(b-a))
        yr = ((y-a)/(b-a))
        len = fromIntegral $ length arr
        i_diameter = (intervalRange i)/len*2
        -- Selects only the elements that intersect with interval `v`
        start_idx = floor $ xr * len
        end_idx = max start_idx $ floorToLowerInt $ yr * len
        sub_arr = take (end_idx - start_idx + 1) (drop start_idx arr)
        -- Zips each selected interval with the corresponding input interval
        sub_arr_w_intervals = zip [Interval (a+(fromIntegral idx)*i_diameter) (a+(fromIntegral idx+1)*i_diameter) | idx <- [start_idx..end_idx]] sub_arr

-- TODO this case happens when not all columns are used in the output, so the output does not matter in this case
fixVariableInterval name _ d = d
--fixVariableInterval name _ _ = error $ "Cannot construct output interval for: " ++ name

-- This function carries the input interval of the specific subdivision
fixVariableInterval_ :: (Show a, RealFrac a) => String -> Interval a -> Interval a -> MultiDimArray a (AF a) -> MultiDimArray a (AF a)
fixVariableInterval_ name i o (Dimension n interval arr) = Dimension n interval $ map (fixVariableInterval_ name i o) arr
fixVariableInterval_ name i o (Elem af) = Elem $ fixEpsilonInterval name (relativeInterval i o) af

-- Same as floor but decrements whole numbers
floorToLowerInt :: (RealFrac a) => a -> Int
floorToLowerInt x
  | (x `mod'` 1) == 0 = -1+floor x
  | otherwise         = floor x
