module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import ConstraintPropagation
import ConstraintsFromInstance
import PropagationConfig
import Debug.Trace
import qualified Control.Exception as Exc

data Flag
    = AttackerPath
    | OutputPath
    | SQLPath
    | IterationNum
    | Help

leakMode :: ReadM LeakMode
leakMode = str >>= \s -> case s of
  "never"     -> return Never
  "always"    -> return Always
  "if-exists" -> return IfExists
  _           -> readerError "Accepted leak modes are: always, never, if-exists"

confParser :: Parser PropagationConfig
confParser = PropagationConfig 
          <$> argument str
              ( metavar "QUERY_FILE"
             <> help "query input file")
          <*> argument str
              ( metavar "ATTACKER_FILE"
             <> help "attacker input file")
          <*> argument str
              ( metavar "INPUT_SCHEMA"
             <> help "input table schemas")
          <*> argument str
              ( metavar "OUTPUT_SCHEMA"
             <> help "output table schema")
          <*> strOption
              ( long "output-file"
             <> short 'o'
             <> metavar "PATH"
             <> value ""
             <> help "output file path")
          <*> switch
              (long "update-output-table"
             <> hidden
             <> help "update contents of the final output table")
          <*> option auto
              ( long "iterations"
             <> short 'i'
             <> metavar "INT"
             <> value 1
             <> showDefault)
          <*> strOption
              ( long "connection"
             <> short 'd'
             <> metavar "STRING"
             <> value ""
             <> help "string containing database connection parameters")
          <*> option leakMode
              ( long "leak-mode"
             <> short 'l'
             <> value IfExists
             <> help ("Specify what to do with rows that might be filtered out\n" ++
                      "Possible values: always, never, if-exists"))

niceError err = "WARNING: constraint propagation experienced the following exception: " ++ show err ++ "\nHence, we proceed with computing constraints directly from the output."

main :: IO ()
main = Exc.catch (loadDataAndRun =<< execParser opts) handler
    where
        handler :: Exc.ErrorCall -> IO ()
        handler err = runWeakerPropagation  =<< (trace (niceError err) $ execParser opts)
        opts = info (confParser <**> helper)
            ( fullDesc
           <> progDesc "Propagate constraints through an SQL query."
           <> header "sql-constraint-propagation - SQL constraint propagation")
