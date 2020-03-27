{-# LANGUAGE OverloadedStrings #-}
module Parser where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as P

import           Database.HsSqlPpp.Parse
import           Database.HsSqlPpp.Syntax
import Database.HsSqlPpp.Dialect

import           System.Directory (doesFileExist)

import           Control.Monad              (void)
import           Data.Char
import           Data.Either
import           Data.List
import           Data.List.Split
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as L
import           Data.Void
import qualified Data.Map                   as M

import           ConstrExpr
import           FilterExpr
import Debug.Trace
type Parser = Parsec Void T.Text
type VarName = String

data AttState a
  = AttRange a a
  | AttExact
  | AttNone
  | AttIntSubSet [Int]
  | AttStrSubSet [String]
  | AttApprox a
  | AttTotal Int
  deriving Show

generateIntermediateTables :: FilePath -> IO [(String,String)]
generateIntermediateTables path = do
    str <- readFile path
    let qs = splitOn ";" str
    return $ concat $ map generateIntermediateTable qs

generateIntermediateTable :: String -> [(String,String)]
generateIntermediateTable q =
    let query = map toLower q in
    if not (isInfixOf "select" query) || not (isInfixOf "from" query) then [] else
    let [insertIntoPart, queryPart] = splitOn "select" query in
    let [selectPart, fromPart] = splitOn "from" queryPart in
    let tableName = if not (isInfixOf "insert" insertIntoPart) || not (isInfixOf "into" insertIntoPart) then ""
                    else filter (\x -> x /= ' ' && x /= '\n') $ (splitOn "into" insertIntoPart) !! 1 in
    let sql = "DROP TABLE IF EXISTS " ++ tableName ++ ";\n\n" ++
              "SELECT " ++ selectPart ++
              " INTO " ++ tableName ++ 
              " FROM " ++ fromPart ++ ";" in
    [(tableName,sql)]

loadConstrASTFromFile :: FilePath -> IO [([ConstrAST Double], FilterExpr Double, [String], [(String,String)], Maybe (String, [String]))]
loadConstrASTFromFile path = do
    str <- readFile path
    let text = L.fromStrict $ T.pack str
    case parseString text of
      Just x  -> return x
      Nothing -> error "Loading expressions from file failed"

-- TODO handle parsing multiple statements
parseString :: L.Text -> Maybe [([ConstrAST Double], FilterExpr Double, [String], [(String,String)], Maybe (String, [String]))]
parseString text = let parsed = parseStatements (defaultParseFlags { pfDialect = postgresDialect }) "hssqlppp_error.log" Nothing text in
                   case parsed of
                     Right xs  -> let ys' = map parseSingleStatement xs in
                                  let ys  = map fromJust $ filter (\x -> case x of Just _ -> Prelude.True; _ -> Prelude.False) ys' in
                                  if (length ys == length ys') then Just ys else Nothing
                     _         -> error ("Parsing failed: " ++ show parsed)

parseSingleStatement :: Statement -> Maybe ([ConstrAST Double], FilterExpr Double, [String], [(String,String)], Maybe (String, [String]))
parseSingleStatement x =
    let (exprs, outputTableData) = fromJust . statementToConstrAST $ x
    in let selectStatement = statementToSelectStatement x
    in Just (exprs, getFilterFromStatement $ selectStatement, getGroupByFromStatement $ selectStatement, getTrefsFromStatement $ selectStatement, outputTableData)

statementToSelectStatement :: Statement -> Statement
statementToSelectStatement (Insert ann _ _ query _) = QueryStatement ann query
statementToSelectStatement query = query

statementToConstrAST :: Statement -> Maybe ([ConstrAST Double], Maybe (String, [String]))
statementToConstrAST s = case s of
                            Insert _ (Name _ [Nmc tableName]) _ (Select{selSelectList=qe}) _ ->
                                let (SelectList _ sil) = qe
                                    (exprList, colNames') = unzip $ map getScalarExpr sil
                                in 
                                let colNames = map fromJust $ filter (\x -> case x of Just _ -> Prelude.True; _ -> Prelude.False) colNames' in
                                let outputTableData = if (length colNames == length colNames') then Just (tableName,colNames) else Nothing in
                                Just (map sqlExprToConstrAST exprList, outputTableData)

                            QueryStatement _ (Select{selSelectList=qe}) ->
                                let (SelectList _ sil) = qe
                                    (exprList, _) = unzip $ map getScalarExpr sil
                                in
                                Just (map sqlExprToConstrAST exprList, Nothing)
                            _ -> Nothing

getFilterFromStatement :: Statement -> FilterExpr Double
getFilterFromStatement s = case s of
                            QueryStatement _ (Select{selWhere=wh}) ->
                                case wh of
                                      Just x  -> sqlExprToFilterExpr x
                                      Nothing -> FilterExpr.True
                            _ -> error "Not a SELECT statement"

getTrefsFromStatement :: Statement -> [(String,String)]
getTrefsFromStatement s = case s of
                            QueryStatement _ (Select{selTref=ts}) -> concat $ map extractTrefs ts
                            _ -> error "Not a SELECT statement"

extractTrefs :: TableRef -> [(String,String)]
extractTrefs (TableAlias _ (Nmc tableAlias) (Tref _ (Name _ [Nmc tableName]))) = [(tableAlias,tableName)]
extractTrefs (Tref _ (Name _ [Nmc tableName])) = [(tableName,tableName)]
extractTrefs (JoinTref _ x1 _ _ _ x2 _) =
    let s1 = extractTrefs x1 in
    let s2 = extractTrefs x2 in
    s1 ++ s2

extractTrefs _ = error "Complex FROM-expressions are not supported yet"


getGroupByFromStatement :: Statement -> [String]
getGroupByFromStatement (QueryStatement _ (Select{selGroupBy=gb})) =
  map (\x -> case x of
    (Identifier _ (Name _ name)) ->  (intercalate "." [n | Nmc n <- name])
    _                            -> error "Unexpected expression in GROUP BY statement")
  gb
getGroupByFromStatement _ = []

-- we extract the column aliases as well to include them in the final output
getScalarExpr :: SelectItem -> (ScalarExpr, Maybe String)
getScalarExpr (SelExp _ expr)                   = (expr, Nothing)
getScalarExpr (SelectItem _ expr (Nmc colName)) = (expr, Just colName)

sqlExprToFilterExpr :: ScalarExpr -> FilterExpr Double
sqlExprToFilterExpr (BinaryOp _ (Name _ [Nmc "and"]) l r) = And
                                               (sqlExprToFilterExpr l)
                                               (sqlExprToFilterExpr r)
sqlExprToFilterExpr (BinaryOp _ (Name _ [Nmc "="]) l r) = CompEQ
                                               (sqlExprToConstrAST l)
                                               (sqlExprToConstrAST r)
sqlExprToFilterExpr (BinaryOp _ (Name _ [Nmc "<="]) l r) = CompLE
                                               (sqlExprToConstrAST l)
                                               (sqlExprToConstrAST r)
sqlExprToFilterExpr (BinaryOp _ (Name _ [Nmc ">="]) l r) = CompGE
                                               (sqlExprToConstrAST l)
                                               (sqlExprToConstrAST r)
sqlExprToFilterExpr (Parens _ expr) = sqlExprToFilterExpr expr
sqlExprToFilterExpr x = CompEQ (sqlExprToConstrAST x) (StrLiteral "\'t\'")

--sqlExprToFilterExpr e = error $ "Expression not supported: " ++ (show e)
sqlExprToConstrAST :: ScalarExpr -> ConstrAST Double
sqlExprToConstrAST (BinaryOp _ (Name _ [Nmc "<@>"]) (App _ (Name _ [Nmc "POINT"]) [x1,x2]) (App _ (Name _ [Nmc "POINT"]) [y1,y2])) =
    let a1 = BinaryApp Add (sqlExprToConstrAST x1) (UnaryApp Neg (sqlExprToConstrAST y1)) in
    let a2 = BinaryApp Add (sqlExprToConstrAST x2) (UnaryApp Neg (sqlExprToConstrAST y2)) in
    let b1 = BinaryApp Pow a1 (Literal 2) in
    let b2 = BinaryApp Pow a2 (Literal 2) in
    BinaryApp Pow (BinaryApp Add b1 b2) (Literal 0.5)

sqlExprToConstrAST (BinaryOp _ (Name _ [Nmc "+"]) l r) = BinaryApp Add
                                               (sqlExprToConstrAST l)
                                               (sqlExprToConstrAST r)
sqlExprToConstrAST (BinaryOp _ (Name _ [Nmc "-"]) l r) = BinaryApp Add
                                               (sqlExprToConstrAST l)
                                               $ UnaryApp Neg (sqlExprToConstrAST r)
sqlExprToConstrAST (BinaryOp _ (Name _ [Nmc "*"]) l r) = BinaryApp Mul
                                               (sqlExprToConstrAST l)
                                               (sqlExprToConstrAST r)
sqlExprToConstrAST (BinaryOp _ (Name _ [Nmc "/"]) l r) = BinaryApp Mul
                                               (sqlExprToConstrAST l)
                                               $ UnaryApp Recip (sqlExprToConstrAST r)
sqlExprToConstrAST (BinaryOp _ (Name _ [Nmc "^"]) l r) = BinaryApp Pow
                                               (sqlExprToConstrAST l)
                                               (sqlExprToConstrAST r)

-- TODO the following points are slightly cheating, we do not process these quite properly yet
-- check if least if implemented correctly
sqlExprToConstrAST (App _ (Name _ [Nmc "CEIL"]) [e])  = sqlExprToConstrAST e
sqlExprToConstrAST (App _ (Name _ [Nmc "FLOOR"]) [e]) = sqlExprToConstrAST e
sqlExprToConstrAST (App _ (Name _ [Nmc "LEAST"]) [l, r]) = BinaryApp Add
                                               (sqlExprToConstrAST l)
                                               (sqlExprToConstrAST r)
                                               --let x = (sqlExprToConstrAST l) in
                                               --let y = (sqlExprToConstrAST r) in
                                               --let w = UnaryApp Abs (BinaryApp Add x (UnaryApp Neg y)) in
                                               --let z = BinaryApp Add (BinaryApp Add x y) (UnaryApp Neg w) in
                                               --BinaryApp Mul (Literal 0.5) z
--

sqlExprToConstrAST (App _ (Name _ [Nmc "POW"]) [l, r]) = BinaryApp Pow
                                               (sqlExprToConstrAST l)
                                               (sqlExprToConstrAST r)
sqlExprToConstrAST (App _ (Name _ [Nmc "LOG"]) [e]) = UnaryApp Log
                                               (sqlExprToConstrAST e)
sqlExprToConstrAST (App _ (Name _ [Nmc "ABS"]) [e]) = UnaryApp Abs
                                               (sqlExprToConstrAST e)
sqlExprToConstrAST (App _ (Name _ [Nmc "COUNT"]) [e]) = Aggregation Count
                                               (sqlExprToConstrAST e)
sqlExprToConstrAST (App _ (Name _ [Nmc "AVG"]) [e]) = Aggregation Avg
                                               (sqlExprToConstrAST e)
sqlExprToConstrAST (App _ (Name _ [Nmc "SUM"]) [e]) = Aggregation Sum
                                               (sqlExprToConstrAST e)
sqlExprToConstrAST (App _ (Name _ [Nmc "MIN"]) [e]) = Aggregation Min
                                               (sqlExprToConstrAST e)
sqlExprToConstrAST (App _ (Name _ [Nmc "MAX"]) [e]) = Aggregation Max
                                               (sqlExprToConstrAST e)
sqlExprToConstrAST (PrefixOp _ (Name _ [Nmc "-"]) e) = UnaryApp Neg
                                               (sqlExprToConstrAST e)
sqlExprToConstrAST (Identifier _ (Name _ name)) = Var (intercalate "." [n | Nmc n <- name])
sqlExprToConstrAST (Parens _ expr) = sqlExprToConstrAST expr
sqlExprToConstrAST (NumberLit _ num) = Literal (read num)
sqlExprToConstrAST (StringLit _ s) = StrLiteral ("\'" ++ s ++ "\'")
-- TODO this does not work if the star is in a non-aggregation statement
sqlExprToConstrAST (Star _) = Literal 0.0

sqlExprToConstrAST e = error $ "Expression not supported: " ++ (show e)

-- Attacker file parsing


parseAttackerFromFile :: FilePath -> IO (M.Map String (AttState Double))
parseAttackerFromFile path = do
    str <- readFile path
    let text = T.pack str
    return $ case runParser fileParser path text of
             Left  x -> error $ parseErrorPretty x
             Right x -> x

parseAttackerFromFileIfExists :: String -> FilePath -> IO (M.Map String (AttState Double))
parseAttackerFromFileIfExists tableName path = do
    fileExists <- doesFileExist path
    res <- if not fileExists then do
               return M.empty
           else do
               str <- readFile path
               let text = T.pack str
               return $ case runParser fileParser path text of
                        Left  x -> error $ parseErrorPretty x
                        Right x -> M.mapKeys (\z -> tableName ++ "." ++ z) x
    return res

identifier :: Parser String
identifier = do
    a  <- letterChar
    at <- many alphaNumCharAndPeriod
    return $ [a] ++ at

alphaNumCharAndPeriod :: Parser Char
alphaNumCharAndPeriod
    = char '.'
  <|> char '_'
  <|> alphaNumChar

fileParser :: Parser (M.Map String (AttState Double))
fileParser = foldr M.union M.empty <$> many attStatement

attStatement :: Parser (M.Map String (AttState Double))
attStatement = do
    name <- identifier
    sc
    state <- varState
    void (P.symbol sc ";")
    return $ M.singleton name state

varState :: Parser (AttState Double)
varState
    = varStateRangePrior
  <|> varStateRange
  <|> varStateSetPrior
  <|> varStateSet
  <|> varStateExact
  <|> varStateNone
  <|> varStateNormal
  <|> varStateApprox
  <|> varStateTotal

-- reads an integer
integer :: Parser Int
integer = P.lexeme sc P.decimal

signedFloat :: Parser Double
signedFloat = try (P.signed sc P.float)
              <|> fmap fromIntegral (P.signed sc integer)

sc :: Parser ()
sc = P.space space1 (P.skipLineComment "//") (P.skipBlockComment "/*" "*/")

-- Statement parsers
-- range x y
varStateRange :: Parser (AttState Double)
varStateRange = do
    ((string $ T.pack "rangeUnif") <|> (string $ T.pack "range"))
    sc
    a <- signedFloat
    sc
    b <- signedFloat
    return $ AttRange a b

-- for a normal distribuition, we take the range that covers 99.5% of inputs, which is mu +- 2*sqrt(2)*sigma
varStateNormal :: Parser (AttState Double)
varStateNormal = do
    string $ T.pack "normal"
    sc
    mean <- signedFloat
    sc
    variance <- signedFloat
    let stdev = if variance < 0 then error " variance of normal distribution should be non-negative" else sqrt variance
    return $ AttRange (mean - 2*sqrt(2)*stdev) (mean + 2*sqrt(2)*stdev)

varStateRangePrior :: Parser (AttState Double)
varStateRangePrior = do
    string $ T.pack "rangePrior"
    sc
    lb <- signedFloat
    sc
    xs <- many varStateRangePriorPair
    return $ AttRange lb (fst (last xs))

varStateRangePriorPair = do
  P.symbol sc "("
  x <- signedFloat
  P.symbol sc ","
  p <- P.float
  P.symbol sc ")"
  return (x,p)

-- set [x,y,..,z]
varStateSet :: Parser (AttState Double)
varStateSet = do
  ((string $ T.pack "setUnif") <|> (string $ T.pack "set"))
  sc
  xs <- many quotedString
  let xs1 = lefts xs
  let xs2 = rights xs
  let y = if length xs1 > 0 && length xs2 > 0 then error $ "Bad constraint input: " ++ show xs
          else if length xs1 > 0 then AttStrSubSet xs1
          else AttIntSubSet xs2
  return y

varStateSetPrior = do
  string $ T.pack "setPrior"
  sc
  zs <- many varStateSetPriorPair
  let (xs,ys) = unzip zs
  let xs1 = lefts xs
  let xs2 = rights xs
  let y = if length xs1 > 0 && length xs2 > 0 then error $ "Bad constraint input: " ++ show zs
          else if length xs1 > 0 then AttStrSubSet xs1
          else AttIntSubSet xs2
  return y

varStateSetPriorPair = do
  P.symbol sc "("
  x <- quotedString
  P.symbol sc ","
  p <- P.float
  P.symbol sc ")"
  return (x,p)

-- total n
varStateTotal :: Parser (AttState Double)
varStateTotal = do
    ((string $ T.pack "totalUnif") <|> (string $ T.pack "total"))
    sc
    n <- integer
    return $ AttTotal n

-- exact
varStateExact :: Parser (AttState Double)
varStateExact = do
    string $ T.pack "exact"
    return AttExact

-- none
varStateNone :: Parser (AttState Double)
varStateNone = do
    string $ T.pack "none"
    return AttNone

-- approx x
varStateApprox :: Parser (AttState Double)
varStateApprox = do
    string $ T.pack "approx"
    sc
    x <- signedFloat
    return $ AttApprox x

quotedString :: Parser (Either String Int)
quotedString = fmap (\s -> Left ("\'" ++ s ++ "\'")) text <|> fmap Right integer

text :: Parser String
text = P.lexeme sc (char '\'' >> manyTill P.charLiteral (char '\''))
