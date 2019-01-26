module Solver(
    Solver (..)
) where

data Solver = Solver {
    name :: String,
    day :: Int,
    year :: Int,
    solve :: String -> [String]
}