{-# LANGUAGE LambdaCase #-}
module Year2018.Day08.Solution(day08) where

import Solver
import Flow
import Text.Parsec

day08 = Solver {
    name = "Memory Maneuver",
    year = 2018,
    day = 8,
    Solver.solve = \case
        Part1 -> Just . show . part1 
        Part2 -> Just . show . part2
}

type Metadata = Int
data Node = Node [Node] [Metadata]

part1 :: String -> Int
part1 input = 
    let
        treeSum :: Node -> Int
        treeSum (Node children metadata) = 
            sum (treeSum <$> children) + sum metadata
    in
        parseInput input |> treeSum

part2 :: String -> Int
part2 input =
    let
        value :: Node -> Int
        value (Node [] metadata) = sum metadata
        value (Node children metadata) = 
            valueAt <$> metadata |> sum
            where 
                valueAt idx = 
                    if length children >= idx 
                        then value $ children !! (idx - 1) 
                        else 0
    in
        parseInput input |> value

parseInput :: String -> Node
parseInput input =
    case parse node "" input of
        Left e -> error $ show e
        Right res -> res
    where
        node :: Parsec String () Node
        node =  do
            childCount <- integer 
            metadDataCount <- integer
            children <- count childCount node 
            metadata <- count metadDataCount integer
            return $ Node children metadata 

        integer :: Parsec String () Int
        integer = read <$> (many digit <* optional (char ' '))