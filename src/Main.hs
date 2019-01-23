module Main where
import Control.Monad
import qualified Year2018.Day01.Solution as Day01
import qualified Year2018.Day02.Solution as Day02
import qualified Year2018.Day03.Solution as Day03
import qualified Year2018.Day04.Solution as Day04

main :: IO ()
main = do
    input <- readFile "src/year2018/day04/input.in"
    forM_ (Day04.solve input) putStrLn
