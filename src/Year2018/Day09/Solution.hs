module Year2018.Day09.Solution(day09) where

import Solver
import Flow
import Text.Parsec
import qualified Data.Map as M
import Data.Maybe(fromJust)
import Debug.Trace
import qualified Data.List.Zipper as Z

day09 = Solver {
    name = "Marble Mania",
    year = 2018,
    day = 9,
    Solver.solve = \case
        Part1 -> Just . show . part1 
        Part2 -> Just . show . part2
}

newtype Point = Point Integer deriving (Eq, Ord)
newtype Marble = Marble Int deriving (Show, Eq, Ord)
newtype PlayerId = PlayerId Int deriving (Show, Eq, Ord)

instance Show Point where
    show (Point p) = show p

cyclicNext :: Z.Zipper Int -> Z.Zipper Int
cyclicNext z = let 
        z' = Z.right z 
    in
        if Z.endp z' then Z.start z else z'

cyclicPrev :: Z.Zipper Int -> Z.Zipper Int
cyclicPrev z = if Z.beginp z then Z.left $ Z.end z else Z.left z

run :: String -> Int -> Point
run input mul = let

    (playerCount, marbleCount) = parseInput input
    lastMarble = marbleCount * mul

    players = cycle (PlayerId <$> [1 .. playerCount])
    marbles = zip players (Marble <$> [1 .. lastMarble]) 

    initialScores :: M.Map PlayerId Point
    initialScores = M.fromList $ (\id -> (PlayerId id, Point 0)) <$> [1 .. playerCount]

    zl =  Z.fromList [0]

    step :: (M.Map PlayerId Point, Z.Zipper Int) -> (PlayerId, Marble) -> (M.Map PlayerId Point, Z.Zipper Int) 
    step (scores, zl) (player, Marble marble) 
        | marble `mod` 23 == 0 = let
                zl' = zl |> cyclicPrev |> cyclicPrev |> cyclicPrev |> cyclicPrev |> cyclicPrev |> cyclicPrev |> cyclicPrev 

                points = toInteger $ marble + Z.cursor zl'
                newZl = Z.delete zl'
                newScores = M.adjust (\(Point p) -> Point $ p + points) player scores
            in 
                (newScores, newZl)

        | otherwise = let 
                newZl = zl |> cyclicNext |> cyclicNext |> Z.insert marble
            in 
               (scores, newZl)

    (scores, _) = foldl step (initialScores, zl) marbles
    in foldr max (Point 0) scores

part1 :: String -> Point
part1 input = run input 1

part2 :: String -> Point
part2 input = run input 100

parseInput :: String -> (Int, Int)
parseInput input = let
        w = words input
    in
        (read $ w !! 0, read $ w !! 6)
