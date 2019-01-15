module Year2018.Day01.Solution(solve) where
import Text.Read
import qualified Data.Set as Set

solve :: String -> [Int]
solve input = [part1 input, part2 input]

part1 :: String -> Int
part1 input =  sum $ parse input

part2 :: String -> Int
part2 input = firstRepetition prefixes Set.empty
    where 
        prefixes = scanl (+) 0 repeatedList
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
