module TestUtils where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers

import qualified Data.Map as M
import GHC.Generics
import Data.List

import qualified IntervalArithmetic as IA
import AffineArithmetic
import ConstrExpr
import qualified FilterExpr as F
import Parser
import PropagationConfig

type AFRat = AF Rational

newtype ValidEnvironment = ValidEnvironment (M.Map String ([Double], AttState Double))
  deriving (Show)

varNames = ["x", "y", "z", "w"]

instance (Fractional a, Arbitrary a) => Arbitrary (AF a) where
  arbitrary = do
                x <- reasonableRational
                vars <- sublistOf varNames
                vals <- infiniteListOf reasonableRational
                return $ AF x (M.fromList $ zip vars vals) 0

instance Arbitrary BinaryFunct where
  arbitrary = elements [ Add
                       , Mul
                     --, Pow
                       ]

instance Arbitrary UnaryFunct where
  arbitrary = elements [ Neg
                     --, Recip
                     --, Log
                       , Abs
                       ]

instance Arbitrary AggFunct where
  arbitrary = elements [ Count
                       , Avg
                       , Sum
                       ]

instance (Ord a, Num a, Arbitrary a) => Arbitrary (F.FilterExpr a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    var <- elements varNames
    v <- arbitrary :: (Ord a1, Num a1, Arbitrary a1) => Gen a1
    elements [ F.And a b
             , F.CompEQ (Var var) (Literal v)
             , F.CompLE (Var var) (Literal v)
             , F.CompGE (Var var) (Literal v)
             ]
  shrink (F.And l r) = [F.True, l, r] ++
                       [F.And l' r' | (l', r') <- shrink (l, r)]
  shrink _ = []

instance (Ord a, Num a, Arbitrary a) => Arbitrary (AttState a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary :: (Ord a1, Num a1, Arbitrary a1) => Gen (Positive a1)
    elements [ AttRange a (a+getPositive b)
             , AttExact
             , AttApprox $ getPositive b
             ]

instance Arbitrary ValidEnvironment where
  arbitrary = do
    let vars = varNames
    len <- sized pure
    vals <- infiniteListOf $ valStatePairs len
    return . ValidEnvironment . M.fromList $ zip vars vals
  shrink (ValidEnvironment env) = ValidEnvironment <$> map f subseqs
    where subseqs = init $ subsequences [0..l-1]
          l = case M.elems env of
                               []     -> -1
                               (x:_) -> length $ fst x
          f il = M.map (\(v,s) -> ([v!!i | i <- il],s)) env

instance Arbitrary LeakMode where
  arbitrary = elements [ Always
                       , IfExists
                       , Never
                       ]

instance (Arbitrary a) => Arbitrary (ConstrAST a) where
  arbitrary = oneof [ sized aggregateConstrAST
                    , sized nonAggregateConstrAST
                    ]
  shrink x = case x of
      BinaryApp op l r -> [l, r] ++
                          [BinaryApp op l' r' | (l',r') <- shrink (l,r)]
      UnaryApp op v    -> [v] ++ [UnaryApp op v' | v' <- shrink v]
      _                -> []

zeroError :: (Num a) => AF a -> AF a
zeroError (AF x xs _) = AF x xs 0
zeroError Empty = Empty

reasonableRational :: (Fractional a) => Gen a
reasonableRational = do
  x <- choose (-100, 100)
  y <- choose (1, 100)
  return $ fromInteger x/fromInteger y

envGen :: (Fractional a) => Gen (M.Map String (AF a))
envGen = do
  vars <- sublistOf varNames
  a    <- infiniteListOf reasonableRational
  b    <- infiniteListOf reasonableRational
  let vals = zip vars $ zip a b
  return $ M.fromList $ map (\(n,(x,xs)) -> (n, AF x (M.singleton n xs) 0)) vals

nonAggregateConstrAST :: (Arbitrary a) => Int -> Gen (ConstrAST a)
nonAggregateConstrAST n = do
  a   <- nonAggregateConstrAST (n-1)
  b   <- nonAggregateConstrAST (n-1)
  bin <- arbitrary
  un  <- arbitrary
  var <- elements varNames
  lit <- arbitrary
  elements [ BinaryApp bin a b
           , UnaryApp un a
           , Literal lit
           , Var var
           ]

aggregateConstrAST :: (Arbitrary a) => Int -> Gen (ConstrAST a)
aggregateConstrAST n = do
  a   <- aggregateConstrAST (n-1)
  b   <- aggregateConstrAST (n-1)
  bin <- arbitrary
  un  <- arbitrary
  agg <- arbitrary
  var <- elements varNames
  lit <- arbitrary
  elements [ BinaryApp bin a b
           , UnaryApp un a
           , Literal lit
           , Aggregation agg (Var var)
           ]

anyConstrAST :: (Arbitrary a) => Int -> Gen (ConstrAST a)
anyConstrAST n = oneof [ aggregateConstrAST n
                       , nonAggregateConstrAST n
                       ]

attStateToInterval :: (Ord a, Fractional a) => AttState a -> [a] -> IA.Interval a
attStateToInterval s []     = IA.Empty
attStateToInterval s (x:xs) = IA.union (attStateToInterval s xs) $ IA.inflate (IA.Interval a b) 0.01
  where i@(IA.Interval a b) = case s of
          AttRange a b -> IA.Interval a b
          AttApprox a  -> IA.Interval (x-a) (x+a)
          AttExact     -> IA.Interval x x

valStatePairs :: Int -> Gen ([Double], AttState Double)
valStatePairs len = do
  v <- infiniteListOf arbitrary
  s <- validState $ take len v
  return (take len v, s)

validState :: [Double] -> Gen (AttState Double)
validState [] = do
  a <- arbitrary :: Gen Double
  b <- arbitrary :: (Ord a1, Num a1, Arbitrary a1) => Gen (Positive a1)
  elements [ AttRange a (a+getPositive b)
           , AttExact
           , AttApprox $ getPositive b
           ]
validState x = do
  a <- arbitrary :: (Ord a1, Num a1, Arbitrary a1) => Gen (Positive a1)
  b <- arbitrary :: (Ord a1, Num a1, Arbitrary a1) => Gen (Positive a1)
  elements [ AttRange ((minimum x) - getPositive a) ((maximum x) + getPositive b)
           , AttExact
           , AttApprox $ getPositive b
           ]
