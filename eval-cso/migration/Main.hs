module Main (main) where

import Prelude

import Db.Migration (runDbMigrations, runSeeder)

main :: IO ()
main = runDbMigrations >> runSeeder
