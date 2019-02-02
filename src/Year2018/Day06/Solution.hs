module Year2018.Day06.Solution(day06) where
import Solver
import Flow
import Control.Monad
import Text.Parsec
import qualified Data.Vector as V 
import qualified Data.List as L
import qualified Data.Set as Set
import Data.Function(on)
import Data.Maybe(mapMaybe)

day06 = Solver {
    name = "Chronal Coordinates",
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
    (border, center) = borderAndCenter coords

    closestCoord :: Coord -> Maybe Coord 
    closestCoord c = 
        let
            (c1:c2:_) = L.sortBy (compare `on` dist c) coords
        in 
            if dist c c1 < dist c c2 
                then Just c1
                else Nothing
    
    infiniteCoords = Set.fromList $ mapMaybe closestCoord border

    closestFiniteCoord :: Coord -> Maybe Coord
    closestFiniteCoord c = do
        closest <- closestCoord c
        if not $ Set.member closest infiniteCoords
            then Just closest
            else Nothing

    biggestArea = center
                    |> mapMaybe closestFiniteCoord  
                    |> L.sort
                    |> L.group
                    |> L.sortBy (compare `on` (\x -> - length x)) 
                    |> head
                    |> length 

    in biggestArea

part2 :: String -> Int
part2 input = let
    coords = parseInput input
    (_, center) = borderAndCenter coords

    closeEnough :: Coord -> Bool
    closeEnough c = sum (map (dist c) coords) < 10000

    res = center
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

borderAndCenter :: [Coord] -> ([Coord], [Coord])
borderAndCenter coords = let
    minX = minimum (getX <$> coords) - 1
    minY = minimum (getY <$> coords) - 1

    maxX = maximum (getX <$> coords) + 1
    maxY = maximum (getY <$> coords) + 1

    border = 
        (Coord <$> [minX] <*> [minY .. maxY]) ++
        (Coord <$> [maxX] <*> [minY .. maxY]) ++
        (Coord <$> [minX .. maxX] <*> [minY]) ++
        (Coord <$> [minX .. maxX] <*> [maxY])

    center = 
        Coord <$> [minX + 1 .. maxX -1] <*> [minY + 1 .. maxY- 1]
    
    in (border, center)

dist :: Coord -> Coord -> Int
dist c1 c2 = abs (getX c1 - getX c2) + abs (getY c1 - getY c2)