module AffineArithmeticSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Data.Map as M

import AffineArithmetic
import TestUtils


spec :: Spec
spec = describe "aa-props" $ do
         prop "add-associative" addAssoc
         prop "add-commutative" addComm
         prop "add-unit" addZero
         prop "mul-associative" mulAssoc
         prop "mul-commutative" mulComm
         prop "mul-unit" mulUnit

addAssoc :: AFRat -> AFRat -> AFRat -> Bool
addAssoc x y z = (x + y) + z == x + (y + z)

addComm :: AFRat -> AFRat -> Bool
addComm x y = x + y == y + x

addZero :: AFRat -> Bool
addZero x = x + 0 == x

mulAssoc :: AFRat -> AFRat -> AFRat -> Bool
mulAssoc x y z = zeroError ((x * y) * z) == zeroError (x * (y * z))

mulComm :: AFRat -> AFRat -> Bool
mulComm x y = zeroError (x * y) == zeroError (y * x)

mulUnit :: AFRat -> Bool
mulUnit x = x * 1 == x
