module Year2018.Day02.Solution(solve) where
import qualified Data.List as List

solve :: String -> [String]
solve input = [show $ part1 input, part2 input]

part1 :: String -> Int
part1 input =  hashCode  where 
    lines' = lines input
    pairs = length $ filter (hasGroupWithLength 2) lines'
    tripples = length $ filter (hasGroupWithLength 3) lines'

    hasGroupWithLength :: Int -> String -> Bool
    hasGroupWithLength r line = 
        let sorted = List.sort line
            groups = List.group sorted
        in List.any (\g -> length g == r) groups

    hashCode = pairs * tripples 

part2 :: String -> String
part2 input = findId $ pairs $ lines input where 

    findId :: [(String, String)] -> String
    findId ((stA, stB) : tail) = 
        let 
            matchingChars = map fst $ filter (uncurry (==)) $ zip stA stB
        in
            if length matchingChars + 1 == length stA 
                then matchingChars 
                else findId tail

    pairs :: [a] -> [(a,a)]
    pairs items = 
        do 
            (x: xs) <- List.tails items
            y <- xs
            return (x, y)