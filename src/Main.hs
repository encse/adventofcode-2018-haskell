module Main where
import Control.Monad
import qualified Year2018.Day02.Solution as Day02

main :: IO ()
main = do
    input <- readFile "src/year2018/day02/input.in"
    putStrLn "Day 02"
    forM_ (Day02.solve input) putStrLn
