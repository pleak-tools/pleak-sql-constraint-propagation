module ConstraintsFromInstance where

import           Data.List
import           ConstraintPropagation(formatOutput2)
import           Database
import           PropagationConfig
import           Schema
import           FilterExpr

processOutput :: Connection -> String -> String -> IO (String)
processOutput conn tableName x = do
    range <- getColumnRangeSS (tableName ++ "." ++ x) FilterExpr.True conn
    let result = formatOutput2 (range, 0)
    return result

-- this function just reads the attribute spans from output table
-- it does not take into account attacker's previous knowledge, so the ranges can be underestimated
runWeakerPropagation :: PropagationConfig -> IO ()
runWeakerPropagation cfg = do

  let dataPath = reverse $ dropWhile (/= '/') (reverse (cfgQueryFile cfg))
  schemaFileContents <- readInput (cfgOutputSchemaFile cfg)
  schemas <- parseSchemas schemaFileContents
  let tableData = map extractTypes schemas
  --putStrLn (show tableData)
  conn <- connection $ cfgConnectionString cfg
  mapM_ (analyzeSchema conn dataPath) tableData

analyzeSchema :: Connection -> String -> (String, [(String,String)]) -> IO()
analyzeSchema conn dataPath (tableName,typeList) = do
  let ys = map fst typeList
  res_show <- mapM (processOutput conn tableName) ys
  let res_str  = (intercalate "\n" $ zipWith (\y r -> tableName ++ "." ++ y ++ " " ++ r) ys res_show) ++ "\n--WARNING: constraints have been computed directly from table data instance"

  -- Output the results
  let outFile =  dataPath ++ tableName ++ ".att"
  --putStrLn (outFile)
  writeFile outFile res_str
  return ()
