module ConstrExpr where

import           VarName

data BinaryFunct
  = Add
  | Mul
  | Pow
  deriving (Show)

data UnaryFunct
  = Neg
  | Recip
  | Log
  | Abs
  deriving (Show)

data AggFunct
  = Count
  | Avg
  | Sum
  | Min
  | Max
  deriving (Show)

data ConstrAST a
  = BinaryApp BinaryFunct (ConstrAST a) (ConstrAST a)
  | UnaryApp UnaryFunct (ConstrAST a)
  | Var VarName
  | Literal a
  | StrLiteral String
  | Aggregation AggFunct (ConstrAST a)
  deriving (Show)

toConstrStatement :: (Show a) => ConstrAST a -> String
toConstrStatement (Var x) = x
toConstrStatement (Literal x) = show x
toConstrStatement (StrLiteral x) = x
toConstrStatement (UnaryApp Neg x) = "-(" ++ toConstrStatement x ++ ")"
toConstrStatement (UnaryApp Recip x) = toConstrStatement x ++ "^(-1)"
toConstrStatement (UnaryApp Log x) = "log(" ++ toConstrStatement x ++ ")"
toConstrStatement (UnaryApp Abs x) = "abs(" ++ toConstrStatement x ++ ")"
toConstrStatement (BinaryApp Add x y) = "(" ++ toConstrStatement x ++ ") + (" ++ toConstrStatement y ++ ")"
toConstrStatement (BinaryApp Mul x y) = "(" ++ toConstrStatement x ++ ") * (" ++ toConstrStatement y ++ ")"
toConstrStatement (BinaryApp Pow x y) = "(" ++ toConstrStatement x ++ ") ^ (" ++ toConstrStatement y ++ ")"
toConstrStatement x = error $ "Unsupported sql expression: " ++ show x

-- Returns True if there are no nested aggregations
valid :: ConstrAST a -> Bool
valid ast = valid' ast False
  where
  valid' (BinaryApp _ l r) n = valid' l n && valid' r n
  valid' (UnaryApp _ x) n = valid' x n
  valid' (Var _) _ = True
  valid' (Literal _) _ = True
  valid' (StrLiteral _) _ = True
  valid' (Aggregation _ x) n
    | n         = False
    | otherwise = valid' x True

-- Checks if the expression has an aggregation function
hasAggregation :: ConstrAST a -> Bool
hasAggregation (BinaryApp _ l r) = hasAggregation l || hasAggregation r
hasAggregation (UnaryApp _ x) = hasAggregation x
hasAggregation (Var _) = False
hasAggregation (Literal _) = False
hasAggregation (StrLiteral _) = False
hasAggregation (Aggregation _ _) = True

defaultAggregation :: (Show a) => ConstrAST a -> Double
defaultAggregation (Aggregation Count _) = 0
defaultAggregation (Aggregation Sum _) = 0
defaultAggregation (Aggregation Avg _) = 0
defaultAggregation (Aggregation Min _) = (1/0)
defaultAggregation (Aggregation Max _) = (-1/0)
defaultAggregation x = error $ "we do not have a default value for non-aggregation queries" ++ show x
