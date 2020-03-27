{-# LANGUAGE OverloadedStrings #-}

module Schema (
  parseSchemas,
  parseSchema,
  extractTypes,
  readInput
  ) where

import Control.Monad
import Data.Data
import Data.List
import Data.Maybe
import Data.Text (Text, pack)
import Database.HsSqlPpp.Annotation
import Database.HsSqlPpp.Catalog
import Database.HsSqlPpp.Dialect
import Database.HsSqlPpp.Parse
import Database.HsSqlPpp.Syntax
import Text.Printf
import System.Exit

import qualified Data.Text.Lazy as T

-- read the DB line by line -- no speacial parsing, assume that the delimiters are whitespaces
readInput :: String -> IO String
readInput path = do
   content <- readFile path
   return content

parseSchemas :: String -> IO [Statement]
parseSchemas s =
  let dialect = postgresDialect in
  let src = T.pack s in
  parseSchema dialect "errorlog.txt" src

parseSchema :: Dialect -> FilePath -> T.Text -> IO [Statement]
parseSchema dialect fp src =
  case parseStatements parseFlags fp Nothing src of
    Left e -> error $ show e
    Right stmts -> return stmts
  where
    parseFlags = defaultParseFlags { pfDialect = dialect }

extractTypes :: Statement -> (String,[(String,String)])
extractTypes (CreateTable _ (Name _ [Nmc tableName]) attributeDefList _ _ _ _) =
    let attrTypes = extractTypesRec attributeDefList in
    (tableName, attrTypes)

extractTypesRec :: [AttributeDef] -> [(String,String)]
extractTypesRec [] = []
extractTypesRec ((AttributeDef _ (Nmc colName) (SimpleTypeName _ (Name _ [Nmc typeName])) _ _) : xs) =
    (colName,typeName) : (extractTypesRec xs)


