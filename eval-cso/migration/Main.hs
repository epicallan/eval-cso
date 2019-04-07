module Main (main) where

import Db.Seed (runSeeder)

main :: IO ()
main = runSeeder
