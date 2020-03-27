module ConstraintPropagation where

import           Data.Char
import           Data.List
import           Data.List.Split
import qualified Data.Map           as M
import qualified Data.Set           as S
import           Text.Printf
import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import           Debug.Trace

import qualified AffineArithmetic   as AA
import           ConstrExpr
import           Database
import           FilterExpr
import qualified IntervalArithmetic as IA
import           IntervalArray
import           Parser             as P
import           PropagationConfig
import           Schema
import qualified SubSet as SS
import           System.Directory (doesFileExist)

type AffineEnvironment a = M.Map VarName (AA.AF a)

data QueryData a b
  = QueryData { qdAttacker   :: M.Map String (AttState a)
              , qdVarRanges  :: M.Map String (SS.Subset a b)
              , qdQueries    :: [ConstrAST a]
              , qdFilter     :: FilterExpr a
              , qdGroupBys   :: [String]
              , qdLeakMode   :: LeakMode
              , qdIterations :: Int
              , qdVals       :: [M.Map String Value]
              }

data RowState
  = NoLeak
  | SomeLeak
  | FullLeak
  deriving (Eq, Show)

data Row a
  = Row { rState :: RowState
        , rEnv   :: M.Map String (IA.Interval a)
        }

instance (Eq a) => Eq (Row a) where
  x == y = rState x == rState y && rEnv x == rEnv y

propagate :: (Ord a, Floating a, Real a, Show a) => [Row a] -> ConstrAST a -> FilterExpr a -> [AA.AF a]
propagate env expr fil = case expr of

  {-
  UnaryApp op e             -> concatMap (\x -> map f $ propagate [x] e fil) env
    where f = case op of
            Neg   -> negate
            Recip -> (1/)
            Log   -> log
            Abs   -> abs
  BinaryApp op l r          -> map (uncurry f) $ concatMap (\x -> zip (propagate [x] l fil) $ propagate [x] r fil) env
    where f = case op of
            Add -> (+)
            Mul -> (*)
            Pow -> (**)

  Aggregation Count (Var n) -> [AA.fromNameAndInterval n $ aggregateCount env n (const 1)]
  Aggregation Sum (Var n)   -> [AA.fromNameAndInterval n $ aggregateCount env n id]
  Aggregation Avg (Var n)   -> zipWith (/) (propagate env (Aggregation Sum $ Var n) fil) $
                               propagate env (Aggregation Count $ Var n) fil
  Aggregation Min (Var n)   -> [AA.fromNameAndInterval n . fromMaybe IA.Empty $ aggregateMin env fil n]
  Aggregation Max (Var n)   -> [AA.fromNameAndInterval n . fromMaybe IA.Empty $ aggregateMax env fil n]

  Var x                     -> if not $ null (map rEnv env)
                               then [AA.fromNameAndInterval x $ head (map rEnv env) M.! x]
                               else error $ "Variable not found: " ++ show x
  Literal x                 -> [realToFrac x]
  x                         -> error $ "This expression is not yet supported: " ++ show x
  -}

  UnaryApp op e             -> map f $ propagate env e fil
    where f = case op of
            Neg   -> negate
            Recip -> (1/)
            Log   -> log
            Abs   -> abs

  BinaryApp op l r          ->
    let envl = propagate env l fil in
    let envr = propagate env r fil in
    zipWith f envl envr
    where f = case op of
            Add -> (+)
            Mul -> (*)
            Pow -> (AA.**)

  -- TODO ideally, we should introduce fresh variable names instead of hard-coded (if we want to use composition)
  Aggregation Count expr'   -> let envTransform = (\rs -> propagate rs expr' fil) in
                               [AA.fromNameAndInterval "y_count" $ aggregateOp envTransform env (const 1) sum 0]
  Aggregation Sum expr'     -> let envTransform = (\rs -> propagate rs expr' fil) in
                               [AA.fromNameAndInterval "y_sum" $ aggregateOp envTransform env AA.toInterval sum 0]
  Aggregation Avg expr'     -> zipWith (/) (propagate env (Aggregation Sum expr') fil) $
                               propagate env (Aggregation Count expr') fil
  Aggregation Min expr'     -> let envTransform = (\rs -> propagate rs expr' fil) in
                               [AA.fromNameAndInterval "y_min" $ aggregateOp envTransform env AA.toInterval (foldr intMin infinity) infinity]
  Aggregation Max expr'     -> let envTransform = (\rs -> propagate rs expr' fil) in
                               [AA.fromNameAndInterval "y_max" $ aggregateOp envTransform env AA.toInterval (foldr intMax (-infinity)) (-infinity)]

  Var x                     -> if not $ null (map rEnv env)
                               then map (AA.fromNameAndInterval x) $ map (\e -> if M.member x (rEnv e) then (M.! x) (rEnv e) else (error $ "Variable not found: " ++ show x)) env
                               else error $ "Variable not found: " ++ show x

  Literal x                 -> replicate (length env) (realToFrac x)
  where
      infinity = 999999999
      intMin (IA.Interval a1 b1) (IA.Interval a2 b2) = IA.Interval (min a1 a2) (min b1 b2)
      intMax (IA.Interval a1 b1) (IA.Interval a2 b2) = IA.Interval (max a1 a2) (max b1 b2)

aggregateOp :: (Ord a, Fractional a, Show a) => ([Row a] -> [AA.AF a]) -> [Row a] -> (AA.AF a -> IA.Interval a) -> ([IA.Interval a] -> IA.Interval a) -> IA.Interval a -> IA.Interval a
aggregateOp envTransform env f aggr ignoredValue =
  aggr $ zipWith m env (((map f) . envTransform) env)
  where m r r' = case rState r of
         NoLeak   -> r'
         FullLeak -> ignoredValue
         SomeLeak -> IA.union ignoredValue r'

{-
aggregateCount :: (Ord a, Fractional a, Show a) => [Row a]  -> String -> (IA.Interval a -> IA.Interval a) -> IA.Interval a
aggregateCount env n f = sum $ map m env
  where m r = case rState r of
         NoLeak   -> f $ rEnv r M.! n
         FullLeak -> 0
         SomeLeak -> IA.union 0 $ f $ rEnv r M.! n

-- TODO Min and max aggregations to be implemented
aggregateMin :: (Ord a, Fractional a, Show a) => [Row a] -> FilterExpr a -> String -> Maybe (IA.Interval a)
aggregateMin env fil n = do
  let (certain, uncertain) = partition (noLeak fil) $ map rEnv env
  let ca = foldr min (1/0) <$> mapM (\e -> IA.low $ e M.! n) certain
  let cb = foldr min (1/0) <$> mapM (\e -> IA.high $ e M.! n) certain
  let ua = foldr min (1/0) <$> mapM (\e -> IA.low $ e M.! n) uncertain
  a <- min <$> ua <*> ca <|> ua <|> ca
  b <- cb
  return $ IA.Interval a b

aggregateMax :: (Ord a, Fractional a, Show a) => [Row a] -> FilterExpr a -> String -> Maybe (IA.Interval a)
aggregateMax = undefined
-}

-- True if the the interval is not affected by the filter
noLeak :: (Ord a, Fractional a, Show a) => FilterExpr a -> M.Map String (SS.Subset a String) -> Bool
noLeak f env = and . M.elems $ M.mapWithKey (\n i -> let (i',b) = applyFilter env f n i in i' == i && b) env

-- True if only a part of the interval leaks
borderLeak :: (Ord a, Fractional a, Show a) => FilterExpr a -> M.Map String (SS.Subset a String) -> Bool
borderLeak f env = not $ noLeak f env || fullLeak f env

-- True if the whole interval is filtered out
fullLeak :: (Ord a, Fractional a, Show a) => FilterExpr a -> M.Map String (SS.Subset a String) -> Bool
fullLeak f env = or . M.elems $ M.mapWithKey (\n i -> let (i',_) = applyFilter env f n i in SS.isEmpty i') env

someLeak :: (Ord a, Fractional a, Show a) => FilterExpr a -> M.Map String (SS.Subset a String) -> Bool
someLeak f env = not $ fullLeak f env

-- Subdivides every range in the environment
-- This results in n environments per range variable
-- These environments are then multiplied together
-- giving m^n environments, where m is the number of
-- range variables in the environment
-- TODO make it possible to specify granularity for each variable separately
subDivideRanges :: (Ord a, Fractional a) => AffineEnvironment a -> Int -> [AffineEnvironment a]
subDivideRanges env n = M.foldrWithKey envMul [M.empty] env
    where envMul k r arr = [unite a b | a <- subdivs, b <- arr]
              where unite a = M.union (M.singleton k a)
                    subdivs = AA.divideAF r n

joinResults :: (Ord a, Num a) => [AA.AF a] -> (IA.Interval a, a)
joinResults []                   = error "Can't join results on an empty list"
joinResults [a@(AA.AF _ _ e)]    = (AA.toInterval a, e)
joinResults (a@(AA.AF _ _ e):xs) = (IA.union b $ AA.toInterval a, max e be)
    where (b, be) = joinResults xs
joinResults (AA.Empty:xs)        = joinResults xs

valToRange :: AttState Double -> Value -> SS.Subset Double String
valToRange AttExact (DoubleVal x)      = SS.createInterval x x
valToRange AttExact (StrVal x)         = SS.createSet [x]
valToRange (AttTotal n) _              = SS.Total n
valToRange AttNone (DoubleVal x)       = SS.createInterval (-1/0) (1/0)
valToRange AttNone (StrVal x)          = SS.Uset
valToRange (AttApprox a) (DoubleVal x) = SS.createInterval (x-a) (x+a)
valToRange (AttRange x y) _            = SS.createInterval x y
valToRange (AttIntSubSet xs) _         = SS.createSet (map show xs)
valToRange (AttStrSubSet xs) _         = SS.createSet xs
valToRange x y                         = error $ "Failed to convert a database value to interval:\n"
                                         ++ show x ++ "\n" ++ show y
-- Is it necessary to look up the variable from the database?
queriableFilter :: AttState a -> Bool
queriableFilter AttExact      = Prelude.True
queriableFilter (AttApprox _) = Prelude.True
queriableFilter _             = Prelude.False

-- Subtracts the part of the interval that is filtered by the filter expression
-- TODO allow more complicated subexpressions (use function propagate to determine their ranges)
-- TODO see if we can easily apply OR operation
applyFilter :: (Ord a, Fractional a, Show a) => M.Map String (SS.Subset a String) -> FilterExpr a -> String -> SS.Subset a String -> (SS.Subset a String, Bool)
applyFilter env fil n i =
 case fil of
  (And l r)    -> (\(i1,b1) (i2,b2) -> (SS.intersect i1 i2, b1 && b2) ) (applyFilter env l n i) (applyFilter env r n i)
  (CompEQ (Var v) (StrLiteral x)) -> if v == n
                                         then (SS.intersect i $ SS.createSet [x], Prelude.True)
                                         else (i,Prelude.True)
  (CompEQ (StrLiteral x) (Var v)) -> applyFilter env (CompEQ (Var v) (StrLiteral x)) n i
  (CompEQ (Var v) (Literal x)) -> if v == n
                                      then (SS.intersect i $ SS.createInterval x x, Prelude.True)
                                      else (i,Prelude.True)

  (CompEQ (Var v1) (Var v2)) -> if v1 == n
                                    then (let j = env M.! v2 in (SS.intersect i j, Prelude.True))
                                    else if v2 == n
                                        then (let j = env M.! v1 in (SS.intersect i j, Prelude.True))
                                        else (i,Prelude.True)

  (CompLE (Var v) (Literal x)) -> if v == n
                                      then (SS.intersect i $ SS.createInterval (-1/0) x,Prelude.True)
                                      else (i,Prelude.True)
  (CompLE (Var v1) (Var v2)) -> if v1 == n
                                    then (let j = env M.! v2 in (SS.intersect i $ SS.createInterval (-1/0) (fromJust $ SS.high j), Prelude.True))
                                    else if v2 == n
                                        then (let j = env M.! v1 in (SS.intersect i $ SS.createInterval (fromJust $ SS.low j) (1/0), Prelude.True))
                                        else (i,Prelude.True)

  (CompGE (Var v) (Literal x)) -> if v == n
                                      then (SS.intersect i $ SS.createInterval x (1/0),Prelude.True)
                                      else (i,Prelude.True)

  (CompGE (Var v1) (Var v2)) -> applyFilter env (CompLE (Var v2) (Var v1)) n i

  (CompEQ v (UnaryApp Neg (Literal x))) -> applyFilter env (CompEQ v . Literal $ negate x) n i
  (CompLE v (UnaryApp Neg (Literal x))) -> applyFilter env (CompLE v . Literal $ negate x) n i
  (CompGE v (UnaryApp Neg (Literal x))) -> applyFilter env (CompGE v . Literal $ negate x) n i

  FilterExpr.True                       -> (i,Prelude.True)

  -- if we are not sure how the filter may affect the variable, let us not set additional constraints
  -- this is why we need an additional variable to determine 'isLeaked'
  --x -> error $ "Unexpected filter statement: " ++ show x
  _ -> (i,Prelude.False)

publicFiltersOnly :: (Ord a, Fractional a, Show a) => M.Map String (AttState Double) -> FilterExpr a -> FilterExpr a
publicFiltersOnly attacker fil =
 case fil of
  (And l r)    -> let fl = publicFiltersOnly attacker l in
                  let fr = publicFiltersOnly attacker r in
                  And fl fr

  (CompEQ (Var v1) (Var v2)) -> if isPublicVar v1 && isPublicVar v2 then fil else FilterExpr.True
  (CompLE (Var v1) (Var v2)) -> if isPublicVar v1 && isPublicVar v2 then fil else FilterExpr.True
  (CompGE (Var v1) (Var v2)) -> if isPublicVar v1 && isPublicVar v2 then fil else FilterExpr.True

  (CompEQ (Var v) _) -> if isPublicVar v then fil else FilterExpr.True
  (CompEQ _ (Var v)) -> if isPublicVar v then fil else FilterExpr.True

  (CompLE (Var v) _) -> if isPublicVar v then fil else FilterExpr.True
  (CompLE _ (Var v)) -> if isPublicVar v then fil else FilterExpr.True

  (CompGE (Var v) _) -> if isPublicVar v then fil else FilterExpr.True
  (CompGE _ (Var v)) -> if isPublicVar v then fil else FilterExpr.True

  FilterExpr.True    -> FilterExpr.True

  -- if we are not sure how the filter may affect the result, we do tot apply this filter
  _ -> FilterExpr.True

 -- we assume that the variables not specified in attacker file are public, to prevent unreasonably large table joins
 where isPublicVar v = not (M.member v attacker) || (case (attacker M.! v) of {AttExact -> Prelude.True; _ -> Prelude.False})

-- Creates a map from variable names to possible value ranges
variableRanges :: Connection -> FilterExpr Double -> M.Map String (AttState Double) -> IO (M.Map String (SS.Subset Double String))
variableRanges conn fil env = sequence $ M.mapWithKey (\name state -> case state of
    AttRange a b -> return $ SS.createInterval a b
    AttExact     -> do
        result <- getColumnRangeSS name fil conn
        return result
    AttNone     -> do
        range <- getColumnRangeSS name fil conn
        let result = case range of
                         SS.Continuous (IA.Interval a b) -> SS.createInterval (-1/0) (1/0)
                         _ -> SS.Uset
        return result
    AttApprox x  -> do
        range <- getColumnRangeSS name fil conn
        let result = case range of
                         SS.Continuous (IA.Interval a b) -> SS.createInterval (a-x) (b+x)
                         _ -> error "Approximation of discrete variables is not supported"
        return result
    -- we currently do not need to constrain ranges of discrete variables, as they are not used inside SELECT anyway
    AttTotal n  -> do
        return $ SS.Total n
    AttIntSubSet xs  -> do
        return $ SS.createSet $ map show xs
    AttStrSubSet xs  -> do
        return $ SS.createSet xs
    --x -> error $ "Unknown attack statement: " ++ show x
  ) env

formatOutput :: (Eq a, Fractional a, PrintfArg a) => (IA.Interval a, a) -> String
formatOutput (IA.Interval a b, e)
  | a == b && e == 0       = printf "exact;" --"exact %.2f;" a
  | otherwise              = printf "range %.2f %.2f (%.2e);" a b e
formatOutput (IA.Empty, _) = printf "exact;"

formatOutput2 :: (Eq a, Fractional a, PrintfArg a) => (SS.Subset a String, a) -> String
formatOutput2 (SS.Continuous (IA.Interval a b), e)
  | a == b && e == 0       = printf "exact;" --"exact %.2f;" a
  | otherwise              = if (a == (-1/0) || b == (1/0)) then  printf "none;" else printf "range %.2f %.2f;" a b

formatOutput2 (SS.Continuous (IA.Empty), _) = printf "exact;" --"empty;"
formatOutput2 (s@(SS.Discrete _), _) = if (length (SS.getSetData s) == 0) then "none;" else "set " ++ intercalate " " (SS.getSetData s) ++ ";"
formatOutput2 (SS.Total n,_)     = "total " ++ show n ++ ";"
formatOutput2 (SS.Uset,_)        = "none;"


groupEnvs :: (Ord a, Ord b) => [[M.Map a b]] -> [a] -> [[M.Map a b]]
groupEnvs envs []     = envs
groupEnvs envs (n:ns) = groupEnvs newEnvs ns
  where newEnvs = concatMap (groupBy eq . sortOn (M.! n)) envs
        eq x y  = x M.! n == y M.! n

runPropagation :: QueryData Double String -> [[(SS.Subset Double String, Double)]]
runPropagation queryData =
    -- Extract data from the QueryData
    let varRanges'     = qdVarRanges queryData
        attacker       = qdAttacker queryData
        rows           = qdVals queryData
        leakMode       = qdLeakMode queryData
        fil            = qdFilter queryData
        groupBys       = qdGroupBys queryData
        queries        = qdQueries queryData
        iterationCount = qdIterations queryData

    -- Convert and filter database rows
        rowsStates     = map (M.mapWithKey (\k v -> (if M.member k attacker then attacker M.! k else AttNone, v))) rows
        rowsEvaled     = map (M.map (uncurry valToRange)) rowsStates
        rowsFiltered   = case leakMode of
          Never  -> filter (noLeak fil) rowsEvaled
          _      -> filter (someLeak fil) rowsEvaled
        rowsApplied = map (\rf -> M.mapWithKey (\n i -> fst $ applyFilter rf fil n i) rf) rowsFiltered

    -- Group the rows
        groupedRows' = groupEnvs [rowsApplied] groupBys

    in let varRanges   = SS.getOnlyNumericRanges varRanges'
    in let groupedRows = map (map SS.getOnlyNumericRanges) groupedRows'

    -- Check whether the queries are aggregate or not and process accordingly
    in if null rowsFiltered
      then [[]]
      else concat $ zipWith (\x x' ->
        if any hasAggregation queries
          then
            -- Aggregate queries
            return $ map (\q -> case q of
                                  Var z -> (SS.joinIntervalList $ map (\env -> if M.member z env then (M.! z) env else error $ "Variable not found: " ++ show z) x',0.0)
                                  _     -> case propagate x q fil of
                                               -- We separate the actual output interval and error
                                               [res@(AA.AF _ _ e)] -> (SS.Continuous $ AA.toIntervalWithoutError res, e)
                                               _                   -> (SS.Continuous $ IA.Empty, 0)
                           ) queries
          else do
            -- Non-aggregate queries
            let arr = fromNamedIntervals varRanges
            let div_arr = map (Row NoLeak) <$> divideMultiDimArray arr iterationCount
            let queryArrs = [case q of
                               Var z -> Right $ (SS.joinIntervalList $ map (\env -> if M.member z env then (M.! z) env else error $ "Variable not found: " ++ show z) x',0.0)
                               _     -> Left  $ (\a -> let r = propagate a q fil in
                                               if not $ null r
                                               then head r
                                               else AA.Empty)
                                               <$> div_arr | q <- queries]
            map (\row -> map (\yy -> case yy of
                                        Left y -> let fixed = fixVariableIntervals (rEnv row) y in
                                                  (SS.Continuous $ outputInterval fixed, maxError fixed)
                                        Right y -> y
                             ) queryArrs
                ) x
      ) (map (map (Row NoLeak)) groupedRows) groupedRows'

-- This function takes a configuration and propagates the given
-- constraints through the SQL query. The results are written to
-- output file (if path given) and then returned by the function
loadDataAndRun :: PropagationConfig -> IO ()
loadDataAndRun cfg = do
  -- Load expressions from the sql query file
  let dataPath = reverse $ dropWhile (/= '/') (reverse (cfgQueryFile cfg))
  zs <- loadConstrASTFromFile $ cfgQueryFile cfg
  let n = length zs

  (intermediateTableNames,intermediateGenQueries) <- fmap unzip $ generateIntermediateTables (cfgQueryFile cfg)
  --putStrLn $ show intermediateTableNames
  --putStrLn $ show intermediateGenQueries

  conn <- connection $ cfgConnectionString cfg
  let qs = if (cfgUpdateOutputTable cfg) then intermediateGenQueries else init intermediateGenQueries
  sendQueriesToDbAndCommit conn qs

  let attOverrideFile   = cfgAttackerFile cfg
  let inputSchemaFile   = cfgInputSchemaFile cfg
  let outputSchemaFile  = cfgOutputSchemaFile cfg
  let finalOutputFile   = cfgOutputFile cfg

  --let outFiles = map (\x -> dataPath ++ x ++ ".att") (init intermediateTableNames) ++ [finalOutputFile]
  let outFiles = map (\x -> dataPath ++ x ++ ".att") intermediateTableNames
  --putStrLn (show outFiles)
  zipWithM_ (processSingleQuery cfg dataPath attOverrideFile inputSchemaFile outputSchemaFile) outFiles zs

processSingleQuery :: PropagationConfig -> String -> String -> String -> String -> String -> ([ConstrAST Double], FilterExpr Double, [String], [(String,String)], Maybe (String, [String])) -> IO ()
processSingleQuery cfg dataPath attOverrideFile inputSchemaFile outputSchemaFile outFile qdata = do

  let (queries, fil, groupBys, inputTables, outputTableData) = qdata

  let inputTableMap = M.fromListWith (++) $ map (\(x,y) -> (y,[x])) inputTables
  let inputAliasMap = M.fromList inputTables
  conn <- connection $ cfgConnectionString cfg

  inputColAliases <- getColNames inputTableMap conn
  let inputColNames = nub $ map (\(x,_,_) -> x) inputColAliases

  -- Load an environment from the attacker file
  att  <- parseAttackerFromFile attOverrideFile
  atts <- mapM (\tn -> parseAttackerFromFileIfExists tn $ dataPath ++ tn ++ ".att") inputColNames

  let attacker' = foldl M.union M.empty (att:atts)
  {-
  traceIO (show inputColNames)
  traceIO (show attacker')
  traceIO ("===")
  -- use input table schema to get the input names
  -- TODO we may want to use these for data types, but it should be optional
  -- TODO we are still relying on existence of intermediate tables
  inputSchemaExists <- doesFileExist inputSchemaFile
  inputColNames  <- if not inputSchemaExists then do
                          -- TODO the attacker file does not necessarily contain all the tables
                          return $ M.keys attacker'
                    else do
                          schemaFileContents <- readInput inputSchemaFile
                          schemas <- parseSchemas schemaFileContents
                          let typeList' = map extractTypes schemas
                          return (concat $ map (\(x,ys) -> map (\(y,_) -> x ++ "." ++ y) ys) typeList')

  let inputColAliases = concat $ map (\x -> let [tableName,varName] = splitOn "." x in
                                            if M.member tableName inputTableMap then
                                                map (\tableAlias -> (tableName, tableAlias, varName)) $ (M.! tableName) inputTableMap
                                            else []) inputColNames
  -}

  -- we declare variables that are missing from attack files as exact
  -- this allows to avoid problem of too many rows in table joins with undefined primary key ranges
  -- it also seems intuitive to let user specify ranges only for sensitive data
  --putStrLn $ show inputTableMap
  --putStrLn $ show inputColNames
  --putStrLn $ show inputColAliases
  let attacker = M.fromList $ map (\(z,x,v) -> let xv = x ++ "." ++ v in
                                               let zv = z ++ "." ++ v in
                                               if M.member zv attacker' then (xv, (M.! zv) attacker')
                                               else (xv, AttExact)) inputColAliases
  --putStrLn $ show attacker

  -- Ensure all the GROUP BY variables are exact
  _ <- mapM_ (\x -> if not (M.member x attacker) then
                        error "Grouping by unknown columns is inaccurate."
                    else
                        case attacker M.! x of
                            AttExact       -> return ()
                            AttIntSubSet _ -> return ()
                            AttStrSubSet _ -> return ()
                            AttTotal _     -> return ()
                            -- TODO we need a datatype check to convert between floats and ints
                            AttRange _ _   -> return ()
                            _              -> error "Grouping by non-discrete columns is inaccurate."
      ) groupBys

  --choose cfgleakRows based on query type: we combine 'Never' and 'Always' to get an interval for aggregations
  let isAggr    = map hasAggregation queries
  let leakTypes = if any hasAggregation queries then [Never,Always] else [cfgLeakRows cfg]

  results <- mapM (\leakType -> do
                         let queryFil = case leakType of
                                 Always -> publicFiltersOnly attacker fil
                                 _      -> fil
                         varRanges <- variableRanges conn queryFil attacker
                         rows      <- getEnvs inputColAliases queryFil conn

                         -- Run propagation
                         let queryData = QueryData { qdFilter = fil
                                                   , qdGroupBys = groupBys
                                                   , qdIterations = cfgIterationCount cfg
                                                   , qdLeakMode = leakType
                                                   , qdVals = rows
                                                   , qdVarRanges = varRanges
                                                   , qdAttacker = attacker
                                                   , qdQueries = queries
                                                   }
                         return $ runPropagation queryData
                 ) leakTypes

  let res' = map (\res -> map (\rs -> let (xs,ys) = unzip rs in (SS.joinIntervalList xs, foldr max 0 ys)) $ transpose res) results
  --traceIO (show res')
  let res   = if (length res' == 1) then
                  head res'
              else
                  let (resNever, resAlways) = (head res', last res') in
                  if (length resNever) == 0 then
                      zipWith (\q (r,e) -> (SS.union r (case q of
                                                            Var z -> SS.Total 0
                                                            _     ->
                                                                let d = defaultAggregation q in
                                                                SS.createInterval d d
                                                       ), e)) queries resAlways
                  else
                      zipWith (\(r1,e1) (r2,e2) -> (SS.union r1 r2, e1 + e2)) resNever resAlways

  -- use output table schema to write down the output
  (tableName,ys) <- case outputTableData of
                        Just z -> do
                                     return z
                        _      -> do
                                     schemaFileContents <- readInput outputSchemaFile
                                     schema <- fmap head $ parseSchemas schemaFileContents
                                     let (tableName,typeList) = extractTypes schema
                                     return (tableName, map fst typeList)

  let res_show = if (length res) == 0 then replicate (length ys) "none;" else map formatOutput2 res
  let res_str  = intercalate "\n" $ zipWith (\y r -> y ++ " " ++ r) ys res_show

  -- Optionally, write the output to a new data table
  --when True $
  --   sendQueriesToDbAndCommit conn qs

  -- Output the results
  writeFile outFile res_str
  --putStrLn (outFile)
  return ()
