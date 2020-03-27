module PropagationConfig where

-- Specifies how to show rows that might not be present in the actual query output
data LeakMode
  = Always   -- Show rows that might be filtered out
  | Never    -- Hide rows that might be filtered out
  | IfExists -- Show rows only if they are actually present in the query output
  deriving (Show)

data PropagationConfig = PropagationConfig{
                                          cfgQueryFile        :: String,
                                          cfgAttackerFile     :: String,
                                          cfgInputSchemaFile  :: String,
                                          cfgOutputSchemaFile :: String,
                                          cfgOutputFile       :: String,
                                          cfgUpdateOutputTable :: Bool,
                                          cfgIterationCount   :: Int,
                                          cfgConnectionString :: String,
                                          cfgLeakRows         :: LeakMode
                                          }

testConfig :: PropagationConfig
testConfig = PropagationConfig{
                                 cfgAttackerFile = "",
                                 cfgQueryFile = "",
                                 cfgInputSchemaFile  = "",
                                 cfgOutputSchemaFile = "",
                                 cfgOutputFile = "",
                                 cfgUpdateOutputTable = False,
                                 cfgIterationCount = 20,
                                 cfgConnectionString = "",
                                 cfgLeakRows = Always
                                 }
