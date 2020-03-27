{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Data.ByteString.Char8     (ByteString, pack, unpack)
import           Data.List
import           Data.List.Split
import qualified Data.Map                  as M
import           Data.Maybe

import qualified Database.PostgreSQL.LibPQ as D

import           FilterExpr
import           IntervalArithmetic
import           VarName
import qualified SubSet as SS

import Text.Read
import Debug.Trace

data Value
  = DoubleVal Double
  | IntVal Int
  | StrVal String
  | Unknown
  deriving (Show)

type Connection = D.Connection
type Table a = [[a]]

-- This arbitrary constant determines how much to inflate the column range
delta :: Double
delta = 0.01

mapElems :: (a -> b) -> Table a -> Table b
mapElems f = map (map f)

mapCols :: ([a] -> [b]) -> Table a -> Table b
mapCols f t = map f (transpose t)

-- Simple connection for testing
connection :: String -> IO Connection
connection conn_str = D.connectdb $ pack conn_str

-- !! No sanitation done
getRows :: [(String,String,String)] -> FilterExpr Double -> Connection -> IO (Table (VarName, Value))
getRows colData f conn = do
    let cols   = map (\(tn,ta,var) -> ta ++ "." ++ var) colData
    let tables = nub $ map (\(tn,ta,var) -> tn ++ " AS " ++ ta) colData
    let sql = sqlcall cols tables
    res <- D.execParams conn (pack sql) [] D.Text
    let jres = fromJust res
    nrows <- D.ntuples jres
    let table = sequence [sequence [parseVal <$> D.getvalue jres r (fromIntegral c) | c <- [0..length cols-1]] | r <- [0..nrows-1]]
    map (zip cols) <$> table
    where sqlcall cols tables = "SELECT " ++ intercalate "," cols ++
                                " FROM " ++ intercalate "," tables ++
                                " WHERE " ++ show f

getEnvs :: [(String,String,String)] -> FilterExpr Double -> Connection -> IO [M.Map VarName Value]
getEnvs c f conn = map M.fromList <$> getRows c f conn

getColNames :: M.Map String [String] -> Connection -> IO [(String,String,String)]
getColNames inputTableMap conn = do
    let tables = map (\t -> "table_name = \'" ++ t ++ "\'") (M.keys inputTableMap)
    let sql = sqlcall tables
    res <- D.execParams conn (pack sql) [] D.Text
    let sres = fromJust res
    nrows <- D.ntuples sres
    tableNames' <- sequence [parseVal <$> D.getvalue sres r 0 | r <- [0..nrows-1]]
    colNames'   <- sequence [parseVal <$> D.getvalue sres r 1 | r <- [0..nrows-1]]
    -- we take tail and init to cut the ' marks around the output values
    let tableNames = map (\(StrVal s) -> tail $ init s) tableNames'
    let colNames   = map (\(StrVal s) -> tail $ init s) colNames'
    return $ concat $ zipWith (\tableName varName -> map (\tableAlias -> (tableName, tableAlias, varName)) ((M.! tableName) inputTableMap)) tableNames colNames             
    where sqlcall :: [String] -> String
          sqlcall tables = "SELECT table_name, column_name FROM information_schema.columns WHERE TABLE_SCHEMA = 'public' AND (" ++ intercalate " OR " tables ++ ")"

-- Gets the interval in which the values of the given variable fall into
-- based on the values in the database
getColumnRangeSS :: VarName -> FilterExpr Double -> Connection -> IO (SS.Subset Double String)
getColumnRangeSS n fil conn = do
    let [t,_] = splitOn "." n
        sql1 = "SELECT MIN(" ++ n ++ "), MAX(" ++ n ++ ") FROM " ++
               t ++ " WHERE " ++ show fil ++ ";"
        sql2 = "SELECT DISTINCT " ++ n ++ " FROM " ++
               t ++ " WHERE " ++ show fil ++ ";"
    res1 <- D.execParams conn (pack sql1) [] D.Text
    let jres = fromJust res1
    res2 <- D.execParams conn (pack sql2) [] D.Text
    let sres = fromJust res2
    nrows <- D.ntuples sres
    xs <- sequence [parseVal <$> D.getvalue sres r 0 | r <- [0..nrows-1]]
    a <- parseVal <$> D.getvalue jres 0 0
    b <- parseVal <$> D.getvalue jres 0 1
    let result = case (a,b) of
                         (DoubleVal x, DoubleVal y) -> SS.Continuous $ inflate (Interval x y) delta
                         _ -> SS.createSet $ map (\(StrVal s) -> s) xs
    return result

getColumnRange :: VarName -> FilterExpr Double -> Connection -> IO (Interval Double)
getColumnRange n fil conn = do
    let [t,_] = splitOn "." n
        sql = "SELECT MIN(" ++ n ++ "), MAX(" ++ n ++ ") FROM " ++
              t ++ " WHERE " ++ show fil ++ ";"
    res <- D.execParams conn (pack sql) [] D.Text
    let jres = fromJust res
    a <- valToDouble . parseVal <$> D.getvalue jres 0 0
    b <- valToDouble . parseVal <$> D.getvalue jres 0 1
    return $ inflate (Interval a b) delta

-- Used to parse results
parseVal :: Maybe ByteString -> Value
parseVal Nothing  = Unknown
parseVal (Just x) = let d' = (readMaybe s :: Maybe Double) in
                    case d' of
                        Just d -> DoubleVal d
                        Nothing -> StrVal ("\'" ++ s ++ "\'")
    where s = unpack x

valToDouble :: Value -> Double
valToDouble (DoubleVal x) = x
valToDouble x             = error $ "Invalid value type: " ++ show x

-- execute a list of queries (e.g. CREATE/DROP TABLE) that change the database and commit the changes
sendQueriesToDbAndCommit :: Connection -> [String] -> IO ()
sendQueriesToDbAndCommit conn qs =
    do mapM_ (\sql -> D.exec conn (pack sql)) qs
