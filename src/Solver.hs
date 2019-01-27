module Solver(
    Part (..),
    Solver (..)
) where

data Part = Part1 | Part2
data Solver = Solver {
    name :: String,
    day :: Int,
    year :: Int,
    solve :: Part -> String -> Maybe String
}