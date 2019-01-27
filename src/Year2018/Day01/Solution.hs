module Year2018.Day01.Solution(day01) where
import Text.Read
import qualified Data.Set as Set
import Solver

day01 = Solver {
    name = "Chronal Calibration",
    day = 1,
    year = 2018,
    solve = \case
        Part1 -> Just . show . part1 
        Part2 -> Just . show . part2
}

part1 :: String -> Int
part1 input =  sum $ parse input

part2 :: String -> Int
part2 input = firstRepetition frequencies Set.empty
    where 
        frequencies = scanl (+) 0 repeatedList
        repeatedList = cycle $ parse input

        firstRepetition (x:xs) seen =
            if Set.member x seen
                then x 
                else firstRepetition xs $ Set.insert x seen

parse :: String -> [Int]
parse =  
    map readInt . lines
    where 
        readInt :: String -> Int
        readInt ('+' : xs) = read xs
        readInt xs = read xs
