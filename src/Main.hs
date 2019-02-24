{-# LANGUAGE BangPatterns #-}

module Main where
import Control.Monad
import System.Console.ANSI
import Text.Printf
import Data.Text(pack, unpack, stripEnd)
import Criterion.Measurement
import Control.Concurrent
import Solver
import Data.Maybe (listToMaybe)
import Control.DeepSeq

import Year2018.Day01.Solution
import Year2018.Day02.Solution
import Year2018.Day03.Solution
import Year2018.Day04.Solution
import Year2018.Day05.Solution
import Year2018.Day06.Solution
import Year2018.Day07.Solution
import Year2018.Day08.Solution

solvers = [day01, day02, day03, day04, day05, day06, day07, day08]

runSolver :: Solver -> IO ()
runSolver solver = do

    input <- unpack . stripEnd . pack <$> readFile (printf "src/year2018/day%02d/input.in" (day solver))
    refout <- readFile $ printf "src/year2018/day%02d/input.refout" (day solver)

    putStrWithColor Vivid White $ "Day " ++ show (day solver) ++ ": " ++ name solver
    putStrLn ""
    putStrLn ""
    
    forM_ [Part1, Part2] $ \part -> do
        initializeTime
        tStart <- getTime
        let !maybeSolution = force $ (solver `solve`) part input
        tEnd <- getTime
        let e = expected refout part
        putStrLnResult maybeSolution e (tEnd - tStart)

    putStrLn ""

    where
        putStrLnResult (Just solution) expected time = do
            let (statusColor, status) = case expected of
                                    Just expected 
                                        | solution == expected -> (Green, "✓")
                                        | otherwise -> (Red, "x")
                                    Nothing -> (Cyan, "?")
           
            putStr "  "
            putStrWithColor Dull statusColor status
            putStr $ " " ++ solution

            let timeColor = if
                                | time < 1000  -> Green
                                | time < 10000 -> Yellow
                                | otherwise    -> Red

            putStrWithColor Dull timeColor $ " (" ++ secs time ++ ")"
            putStrLn ""

        putStrLnResult _ _ _ = return ()

        putStrWithColor intensity color st = do
            setSGR [SetColor Foreground intensity color]
            putStr st
            setSGR [Reset]

expected :: String -> Part -> Maybe String
expected input p = case p of
    Part1 -> if length (lines input) > 0 then Just (lines input !! 0) else Nothing
    Part2 -> if length (lines input) > 1 then Just (lines input !! 1) else Nothing

main :: IO ()
main = forM_ solvers runSolver