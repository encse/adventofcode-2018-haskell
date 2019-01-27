module Year2018.Day05.Solution(day05) where
import Data.Char(toUpper)
import Solver

day05 = Solver {
    name = "Alchemical Reduction",
    year = 2018,
    day = 5,
    solve = \case
        Part1 -> Just . show . part1 
        Part2 -> Just . show . part2
}

part1 :: String -> Int
part1 = length . fullyReact

part2 :: String -> Int
part2 input = minimum candidates
    where
        candidates = do 
            char <- ['A'..'Z']
            let filtered = filter (\ch -> toUpper ch /= char) input
            return $ length $ fullyReact filtered

fullyReact :: String -> String 
fullyReact = foldr add []
    where
        add :: Char -> String -> String
        add ch [] = [ch]
        add ch xxs@(x:xs)
            | reacts ch x = xs
            | otherwise   = ch : xxs

            where 
                reacts chA chB = chA /= chB && toUpper chA == toUpper chB
