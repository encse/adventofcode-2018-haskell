module Year2018.Day06.Solution(day06) where
import Text.Parsec
import Solver

import Flow
import qualified Data.Matrix.Mutable as MM
import qualified Data.Matrix as M
import qualified Data.Vector as V 

import Data.List as L
import Data.Function(on)
import Control.Monad
import Data.Maybe(mapMaybe)
import qualified Data.Set as Set

day06 = Solver {
    name = "...",
    year = 2018,
    day = 6,
    Solver.solve = \case
        Part1 -> Just . show . part1 
        Part2 -> Just . show . part2
}

data Coord = Coord
    { getX :: Int
    , getY :: Int
    } deriving (Show, Eq, Ord)

part1 :: String -> Int
part1 input = let
    coords = parseInput input

    minX = minimum (map getX coords) - 1
    minY = minimum (map getY coords) - 1

    maxX = maximum (map getX coords) + 1
    maxY = maximum (map getY coords) + 1

    frame :: [Coord]
    frame = 
        (Coord <$> [minX] <*> [minY .. maxY]) ++
        (Coord <$> [maxX] <*> [minY .. maxY]) ++
        (Coord <$> [minX .. maxX] <*> [minY]) ++
        (Coord <$> [minX .. maxX] <*> [maxY])

    center :: [Coord]
    center = Coord <$> [minX + 1 .. maxX -1] <*> [minY + 1 .. maxY- 1]

    closestCoord :: Coord -> Maybe Coord 
    closestCoord c = 
        let
            (c1:c2:_) = L.sortBy (compare `on` dist c) coords
        in 
            if dist c c1 < dist c c2 
                then Just c1
                else Nothing

    infiniteCoords = Set.fromList $ mapMaybe closestCoord frame

    closestFiniteCoord :: Coord -> Maybe Coord
    closestFiniteCoord c = do
        closest <- closestCoord c
        if not $ Set.member closest infiniteCoords
            then Just closest
            else Nothing

    biggestArea = mapMaybe closestFiniteCoord center 
                    |> L.sort
                    |> L.group
                    |> L.sortBy (compare `on` (\x -> - length x)) 
                    |> head
                    |> length 

    in biggestArea


part2 :: String -> Int
part2 input = let
    coords = parseInput input

    minX = minimum (map getX coords)
    minY = minimum (map getY coords)

    maxX = maximum (map getX coords)
    maxY = maximum (map getY coords)

    area :: [Coord]
    area = Coord <$> [minX .. maxX] <*> [minY .. maxY]
    
    closeEnough :: Coord -> Bool
    closeEnough c = sum (map (dist c) coords) < 10000

    res = area
            |> L.filter closeEnough
            |> length
    in res

parseInput :: String -> [Coord]
parseInput input =
    case parse coords "" input of
        Left e -> error $ show e
        Right coords -> coords
    where
        coords :: Parsec String () [Coord]
        coords = many coord

        coord :: Parsec String () Coord
        coord = Coord <$> 
            (integer <* string ", ") <*> 
            (integer <* optional (char '\n'))

        integer :: Parsec String () Int
        integer = read <$> many digit

dist :: Coord -> Coord -> Int
dist c1 c2 = abs (getX c1 - getX c2) +  abs (getY c1 - getY c2)