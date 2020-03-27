module FilterExpr where

import ConstrExpr

data FilterExpr a
  = And (FilterExpr a) (FilterExpr a)
  | CompEQ (ConstrAST a) (ConstrAST a)
  | CompLE (ConstrAST a) (ConstrAST a)
  | CompGE (ConstrAST a) (ConstrAST a)
  | True
  | False

instance (Show a) => Show (FilterExpr a) where
  show (And l r)       = (show l) ++ " AND " ++ (show r)
  show (CompEQ l r)    = (toConstrStatement l) ++ " = " ++ (toConstrStatement r)
  show (CompGE l r)    = (toConstrStatement l) ++ " >= " ++ (toConstrStatement r)
  show (CompLE l r)    = (toConstrStatement l) ++ " <= " ++ (toConstrStatement r)
  show FilterExpr.True = "1=1"
  show x               = error $ "Unsupported filter expression: " ++ (show x)
