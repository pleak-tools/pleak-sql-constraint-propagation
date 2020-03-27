module ConstraintPropagationSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Test.QuickCheck

import qualified Data.Map as M
import Data.List

import ConstraintPropagation
import ConstrExpr
import qualified FilterExpr as F
import AffineArithmetic
import qualified IntervalArithmetic as IA
import qualified SubSet as SS
import PropagationConfig
import Database
import Parser

import TestUtils

spec :: Spec
spec = describe "constraint-propagation" $ do
         prop "every subdivision is contained within the whole" $
                         forAll (choose (1, 10)) $
           \i         -> forAll envGen $
           \af        -> subdivContain af i
         prop "every output interval contains the exact output" $
                          forAll (choose (1, 10)) $
           \i env expr -> propagationContain env expr F.True i
         prop "never is contained within if-exists is contained within always" $
                              forAll (choose (1, 10)) $
           \i env fil expr -> leakModeContain env expr fil i
         prop "no empty rows" $
                              forAll (choose (1, 10)) $
           \i env fil expr -> noEmptyRows env expr fil i


subdivContain :: M.Map String (AF Rational) -> Int -> Bool
subdivContain af n = all varContains $ M.keys af
  where subdivs = map (M.map zeroError) $ subDivideRanges af n
        varContains k = all (\a -> toInterval (a M.! k) <= toInterval (af M.! k)) subdivs

propagationContain :: ValidEnvironment -> ConstrAST Double -> F.FilterExpr Double -> Int -> Bool
propagationContain (ValidEnvironment env) expr fil i = res >= ex
  where vals = M.map fst env
        conf = QueryData{ qdAttacker = M.map snd env
                        , qdVarRanges = M.map SS.Continuous $ M.map (uncurry (flip attStateToInterval)) env
                        , qdQueries = [expr]
                        , qdFilter = fil
                        , qdGroupBys = []
                        , qdLeakMode = IfExists
                        , qdIterations = i
                        , qdVals = flippedVals vals
                        }
        exConf = conf {qdAttacker = M.map (const AttExact) env}
        res = case map fst $ head $ runPropagation conf of
                []    -> [[]]
                (x:_) -> [[x]]
        ex  = case map fst $ head $ runPropagation exConf of
                []    -> [[]]
                (x:_) -> [[x]]

leakModeContain :: ValidEnvironment -> ConstrAST Double -> F.FilterExpr Double -> Int -> Bool
leakModeContain (ValidEnvironment env) expr fil i =
  if null alw
    then null ife && null nev
    else alw >= ife && ife >= nev
  where vals = M.map fst env
        flipVals = flippedVals vals
        conf = QueryData{ qdAttacker = M.map snd env
                        , qdVarRanges = M.map SS.Continuous $ M.map (uncurry (flip attStateToInterval)) env
                        , qdQueries = [expr]
                        , qdFilter = fil
                        , qdGroupBys = []
                        , qdLeakMode = Always
                        , qdIterations = i
                        , qdVals = flipVals
                        }
        confNever = conf {qdLeakMode = Never}
        confIfe = conf {qdLeakMode = IfExists}
        alw = map fst $ head $ runPropagation conf
        nev = map fst $ head $ runPropagation confNever
        ife = map fst $ head $ runPropagation confIfe

flippedVals :: (Ord k) => M.Map k [Double] -> [M.Map k Value]
flippedVals vals = map M.fromList . transpose . map (\(s,v) -> map (\x -> (s,DoubleVal x)) v) $ M.toList vals

noEmptyRows :: ValidEnvironment -> ConstrAST Double -> F.FilterExpr Double -> Int -> LeakMode -> Bool
noEmptyRows (ValidEnvironment env) expr fil i lm = case res of
                                                     [] -> True
                                                     x  -> all (\r -> not $ SS.isEmpty (fst r)) x
  where vals = M.map fst env
        conf = QueryData{ qdAttacker = M.map snd env
                        , qdVarRanges = M.map SS.Continuous $ M.map (uncurry (flip attStateToInterval)) env
                        , qdQueries = [expr]
                        , qdFilter = fil
                        , qdGroupBys = []
                        , qdLeakMode = lm
                        , qdIterations = i
                        , qdVals = flippedVals vals
                        }
        res = case runPropagation conf of
                []    -> []
                (x:_) -> x

valuesFiltered :: F.FilterExpr Double -> AttState Double -> Int -> Bool
valuesFiltered = undefined
