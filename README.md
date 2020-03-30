# Pleak SQL constraint-propagation

This program is used to estimate the results of an SQL SELECT query given some approximate knowledge about table rows.

## Installation

Run the following commands:
```
# cabal sandbox init
# cabal install --only-dependencies
# cabal configure
# cabal build
# cabal test
```

Example run: ./dist/build/sql-constraint-propagation/sql-constraint-propagation --connection dbname=banach --leak-mode if-exists -o output.att src/psql/cat_group_query.sql src/psql/cat_attacker.att src/psql/cat_schema.sql src/psql/cat_group_schema.sql
