module Main (main) where

import Db.Migration (runDbMigrations, runSeeder)

main :: IO ()
main = runDbMigrations >> runSeeder
