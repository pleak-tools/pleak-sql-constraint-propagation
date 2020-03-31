# Pleak SQL constraint propagation

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

If PostgreSQL has not been installed yet, it needs to be done before running. The analyser has been tested with versions from 9.5.13 to 10.

After PostgreSQL has been installed, it is necessary to create a database that will be used by the tool. For compatibility with pleak-backend, the database name should be 'ga_propagation'. Permissions on 'ga_propagation' should be given to the user 'ga_propagation', whose password is the same as used inside "pleak-tools/pleak-backend/src/main/java/com/naples/rest/GuessingAdvantageService.java", which is 'ceec4eif7ya' by default. If any other database or user names are used, they should be changed in "GuessingAdvantageService.java" as well. Here is an example of how to do it with Ubuntu system:

1. Create the ga_propagation user.

    USERNAME@xxxx:~$ sudo -u postgres -i
    (prompts USERNAME's password)
    postgres@xxxx:~$ createuser --interactive --pwprompt
    Enter name of role to add: ga_propagation
    Enter password for new role: ceec4eif7ya
    Enter it again: 
    Shall the new role be a superuser? (y/n) n
    Shall the new role be allowed to create databases? (y/n) n
    Shall the new role be allowed to create more new roles? (y/n) n

2. Create the ga_propagation database.

    postgres@xxxx:~$ psql
    postgres=# create database ga_propagation;
    CREATE DATABASE
    postgres=# \c ga_propagation
    You are now connected to database "ga_propagation" as user "postgres".

    ga_propagation=# create extension cube;
    CREATE EXTENSION
    ga_propagation=# create extension earthdistance;
    CREATE EXTENSION
    ga_propagation=# \q

The tool assumes that the contents of input tables are uploaded to the database by some other application. The example runs will fail if there is no suitable data, i.e. no tables specified in the input schema file.

Example run: ./dist/build/sql-constraint-propagation/sql-constraint-propagation --connection "host=localhost dbname=ga_propagation user=ga_propagation password=ceec4eif7ya" --leak-mode if-exists -o output.att src/psql/cat_group_query.sql src/psql/cat_attacker.att src/psql/cat_input_schema.sql src/psql/cat_group_output_schema.sql

