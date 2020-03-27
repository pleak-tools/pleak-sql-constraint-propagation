module Tests where

import Test.HUnit as T
import AffineArithmetic as AA
import IntervalArithmetic as IA
import ConstraintPropagation
import Parser

import Data.Map as M
import Data.Text.Lazy
import Control.Monad

import qualified Database.PostgreSQL.LibPQ as D
import qualified Data.ByteString.Char8 as B (unpack, pack, ByteString) 

errorMargin = 0.05
af0 = AF 3 (M.fromList [("x",2)]) 0
af1 = AF 0 (M.fromList [("y",5)]) 0
af2 = AF 1.5 (M.fromList [("x", 0.3),("y", 0.2)]) 0
af3 = AF 2 (M.fromList [("x",1)]) 0
expr0 = sqlExpr "POW(x.x, 2) - 3*x.x + 1"

runTests :: IO Counts
runTests = runTestTT tests
    where tests = 
            T.test [
                 -- Tests for checking whether the output encloses the actual range
                 "inv af0" ~: assertWithin (Interval 0.2 1) $ toInterval $ AA.inv af0,

                 "af1 * af1" ~: assertWithin (Interval (-25) 25) $ toInterval $ af1 * af1,
                 "af1 * af0" ~: assertWithin (Interval (-25) 25) $ toInterval $ af1 * af0,

                 "af2**3" ~: assertWithin (Interval 1 8) $ toInterval $ af2 AA.** 3,
                 "af1**2" ~: assertWithin (Interval 0 9) $ toInterval $ af1 AA.** 2,

                 "log af2" ~: assertWithin (Interval 0 0.693) $ toInterval $ afLog af2,

                 "af2**2-3*af2+1" ~: 
                     assertWithin (Interval (-1.25) (-1)) $ toInterval $ (af2 AA.** 2)-3*af2+1,

                 -- Tests for checking whether error is within some bounds
                 "af1*af1" ~: assertWithinBounds (Interval (-25) 25) errorMargin $ toInterval $ af1 * af1
                 ]

-- Utilities
assertWithin :: (Ord a, Show a) => IA.Interval a -> IA.Interval a -> Assertion
assertWithin expect actual = unless (expect <= actual) (assertFailure msg)
    where msg = (show expect) ++ " is not within " ++ (show actual)

assertWithinBounds :: (Ord a, Show a, Num a) => IA.Interval a -> a -> IA.Interval a -> Assertion
assertWithinBounds expect err actual = do
    assertWithin expect actual
    unless (actual <= outer) (assertFailure msg)
      where msg = (show actual) ++ " is not within required range " ++ (show outer)
            outer = (\(Interval a b) -> Interval (a-err) (b+err)) expect

getInterval :: (IA.Interval a, a) -> IA.Interval a
getInterval (i,_) = i

sqlExpr :: String -> ConstrExpr
sqlExpr s = case (parseString $ pack ("SELECT " ++ s ++ " FROM x;")) of
          Just (x,_)  -> x !! 0
          Nothing -> error "Parsing failed"
