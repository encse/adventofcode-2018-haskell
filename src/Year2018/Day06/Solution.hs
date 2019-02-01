{-# LANGUAGE BangPatterns #-}

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
import Debug.Trace
import Control.DeepSeq

-- data Part = Part1 | Part2
-- data Solver = Solver {
--     name :: String,
--     day :: Int,
--     year :: Int,
--     solve :: Part -> String -> Maybe String
-- }

day06 = Solver {
    name = "Chronal Coordinates",
    year = 2018,
    day = 6,
    Solver.solve = \case
        Part1 -> Just . show . part1 
        Part2 -> Just . show . part2
}

data Coord = Coord !Int !Int deriving (Show, Eq, Ord)
    -- { getX :: Int
    -- , getY :: Int
    -- } deriving (Show, Eq, Ord)
getX (Coord x y) = x
getY (Coord x y) = y


frame :: [Coord] -> [Coord]
frame coords = let
   -- 
    xs = getX <$> coords 
    ys = getY <$> coords 

    minX = (minimum xs) - 1
    minY = (minimum ys) - 1


    maxX = (maximum xs) + 1
    maxY = (maximum ys) + 1

    frame' = [minX, minY, maxX, maxY] `deepseq`
        (Coord <$> [minX] <*> [minY .. maxY]) ++
        (Coord <$> [maxX] <*> [minY .. maxY]) ++
        (Coord <$> [minX .. maxX] <*> [minY]) ++
        (Coord <$> [minX .. maxX] <*> [maxY])

    in  frame'
     


thecoords = [Coord 132 308,Coord 325 300,Coord 310 231,Coord 177 248,Coord 111 304,Coord 65 135,Coord 227 116,Coord 60 80,Coord 182 353,Coord 60 42,Coord 314 164,Coord 142 50,Coord 90 266,Coord 234 219,Coord 68 121,Coord 168 153,Coord 258 50,Coord 354 92,Coord 126 154,Coord 303 324,Coord 90 47,Coord 236 316,Coord 316 217,Coord 180 110,Coord 70 300,Coord 256 221,Coord 56 256,Coord 235 190,Coord 56 197,Coord 168 145,Coord 250 117,Coord 107 77,Coord 259 156,Coord 188 301,Coord 183 76,Coord 92 224,Coord 41 113,Coord 343 90,Coord 162 176,Coord 186 77,Coord 312 134,Coord 89 98,Coord 191 313,Coord 68 225,Coord 85 273,Coord 96 161,Coord 260 93,Coord 343 153,Coord 247 327,Coord 151 197]

parseInput :: String -> [Coord]
parseInput input = thecoords

part1 :: String -> Int
part1 input = let
    coords = parseInput input
    frame' = frame coords
    --frame' = frame coords

    minX = minimum (getX <$> coords) - 1
    minY = minimum (getY <$> coords) - 1

    maxX = maximum (getX <$> coords) + 1
    maxY = maximum (getY <$> coords) + 1

    -- frame' = (Coord <$> [minX] <*> [minY .. maxY]) ++
    --     (Coord <$> [maxX] <*> [minY .. maxY]) ++
    --     (Coord <$> [minX .. maxX] <*> [minY]) ++
    --     (Coord <$> [minX .. maxX] <*> [maxY])

   

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

    infiniteCoords = Set.fromList $ mapMaybe closestCoord frame'

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

    -- [Coord 132 308,Coord 325 300,Coord 310 231,Coord 177 248,Coord 111 304,Coord 65 135,Coord 227 116,Coord 60 80,Coord 182 353,Coord 60 42,Coord 314 164,Coord 142 50,Coord 90 266,Coord 234 219,Coord 68 121,Coord 168 153,Coord 258 50,Coord 354 92,Coord 126 154,Coord 303 324,Coord 90 47,Coord 236 316,Coord 316 217,Coord 180 110,Coord 70 300,Coord 256 221,Coord 56 256,Coord 235 190,Coord 56 197,Coord 168 145,Coord 250 117,Coord 107 77,Coord 259 156,Coord 188 301,Coord 183 76,Coord 92 224,Coord 41 113,Coord 343 90,Coord 162 176,Coord 186 77,Coord 312 134,Coord 89 98,Coord 191 313,Coord 68 225,Coord 85 273,Coord 96 161,Coord 260 93,Coord 343 153,Coord 247 327,Coord 151 197]


    -- case parse coords "" input of
    --     Left e -> error $ show e
    --     Right coords -> coords
    -- where
    --     coords :: Parsec String () [Coord]
    --     coords = many coord

    --     coord :: Parsec String () Coord
    --     coord = Coord <$> 
    --         (integer <* string ", ") <*> 
    --         (integer <* optional (char '\n'))

    --     integer :: Parsec String () Int
    --     integer = read <$> many digit

dist :: Coord -> Coord -> Int
dist c1 c2 = abs (getX c1 - getX c2) +  abs (getY c1 - getY c2)


    -- [Coord x y | let x = minX, y <- [minY .. maxY]]

    -- where
    --     fast = False
    --     minX = if fast then 40 else minimum (getX <$> coords) - 1
    --     minY = if fast then 41 else minimum (getY <$> coords) - 1

    --     maxX = if fast then 355 else maximum (getX <$> coords) + 1
    --     maxY = if fast then 354 else maximum (getY <$> coords) + 1


    --in  -- error $ show [minX, minY, maxX, maxY]
        
        -- (Coord <$> [minX] <*> [minY .. maxY]) 
        -- (Coord <$> [maxX] <*> [minY .. maxY]) ++
        -- (Coord <$> [minX .. maxX] <*> [minY]) ++
        -- (Coord <$> [minX .. maxX] <*> [maxY])