module Main where
import Control.Monad
import qualified Year2018.Day01.Solution as Day01
import qualified Year2018.Day02.Solution as Day02
import qualified Year2018.Day03.Solution as Day03

main :: IO ()
main = do
    input <- readFile "src/year2018/day03/input.in"
    putStrLn "Day 03"
    forM_ (Day03.solve input) putStrLn
