{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where
import Control.Monad

import Year2018.Day01.Solution
import Year2018.Day02.Solution
import Year2018.Day03.Solution
import Year2018.Day04.Solution
import System.Console.ANSI
import Solver
import Text.Printf
import Criterion.Measurement
import Control.Concurrent

solvers = [day01, day02, day03, day04]

runSolver :: Solver -> IO ()
runSolver solver = do
    input <- readFile $ printf "src/year2018/day%02d/input.in" (day solver)
    refout <- readFile $ printf "src/year2018/day%02d/input.refout" (day solver)
    let solution = solver `solve` input
    let expecteds = lines refout
    
    setSGR [SetColor Foreground Vivid White]
    putStrLn $ "Day " ++ show (day solver) ++ ": " ++ name solver
    setSGR [Reset]
    putStrLn ""
    
    let check = zip solution ((Just <$> expecteds) ++ repeat Nothing)
    forM_ check $ \(a, me) -> do
            let (color, status) = case me of
                                    Just e 
                                        | a == e -> (Green, "✓")
                                        | otherwise -> (Red, "x")
                                    Nothing -> (Cyan, "?")

            setSGR [SetColor Foreground Dull color]
            putStr $ "  " ++ status
            setSGR [Reset]
            putStrLn $ " " ++ a

    -- putStr $ " " ++ show (secs executionTime) ++ "seconds"
    putStrLn ""

main :: IO ()
main = forM_ solvers runSolver