module Main where
import Control.Monad
import qualified Year2018.Day01.Solution as Day01

main :: IO ()
main = do
    input <- readFile "src/year2018/day01/input.in"
    putStrLn "Day 01"
    forM_ (Day01.solve input) print
