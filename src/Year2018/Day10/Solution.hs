{-# LANGUAGE NamedFieldPuns #-}

module Year2018.Day10.Solution(day10) where

import Solver
import Flow
import qualified Text.Parsec as P
import Control.Monad.State
import Control.Monad.Loops
import qualified Data.Map as M
import Data.Maybe (fromJust)

day10 = Solver {
    name = "The Stars Align",
    year = 2018,
    day = 10,
    Solver.solve = \case
        Part1 -> Just . part1 
        Part2 -> Just . show . part2
}

data Particle = Particle {
    x :: Int,
    y :: Int,
    vx :: Int,
    vy :: Int
}

type Particles =  [Particle]
data Rect = Rect {
    minX :: Int,
    minY :: Int,
    maxX :: Int,
    maxY :: Int
}

data MyState = MyState {
    particles :: Particles,
    rect :: Rect,
    t :: Int
}

move :: Particle -> Particle
move p@Particle {x, y, vx, vy} = p {x = x + vx, y = y + vy}

boundingBox :: Particles -> Rect
boundingBox ps = Rect {
    minX = minimum $ x <$> ps,
    maxX = maximum $ x <$> ps,
    minY = minimum $ y <$> ps,
    maxY = maximum $ y <$> ps
}

area :: Rect -> Int 
area Rect {minX, minY, maxX, maxY} = (maxX - minX + 1) * (maxY - minY + 1)

step :: State MyState Bool
step = do
    ps <- gets particles
    t <- gets t
    r <- gets rect

    let ps' =  move <$> ps
    let r' = boundingBox ps'
    
    when (area r' < area r) $
        modify $ \s -> s {
            particles = ps',
            rect = r',
            t = t + 1
        } 

    return $ area r' < area r

ocr :: Particles -> String
ocr ps = res 
    where
        Rect {minX, maxX, minY, maxY} = boundingBox ps

        height = maxY  - minY + 1
        width = maxX - minX + 1

        dict = M.fromList [
                (0x384104104145138, 'J'),
                (0xF4304104F0C31BA, 'G'),
                (0x1F430C3F430C30FC, 'B'),
                (0xF430410410410BC, 'C'),
                (0x1F8208421084107E, 'Z'),
                (0x114517D145144, 'H'),
                (0x1841041041060, 'I')
            ]

        coords :: Int -> [(Int, Int)]
        coords ch = [(irow, icol) | irow <- [minY .. maxY], i <- [0 .. 5], let icol = minX + (ch * 8) + i]

        hasParticleAt :: (Int, Int) -> Bool
        hasParticleAt (irow, icol) = 
            not . null $ filter (\Particle {x,y} -> x == icol && y == irow) ps

        hash ch = foldl step 0 (coords ch) 
            where
                step hash (irow, icol)
                    | hasParticleAt (irow, icol) = (hash + 1) * 2 
                    | otherwise = hash * 2

        res = fromJust . (`M.lookup` dict) . hash <$> [0 .. (width `div` 8)]


part1 :: String -> String
part1 input = execState (iterateWhile id step) (parseInput input) |> particles |> ocr

part2 :: String -> Int
part2 input = execState (iterateWhile id step) (parseInput input) |> t

parseInput :: String -> MyState
parseInput input =
    case P.parse particles "" input of
        Left e -> error $ show e
        Right particles -> MyState {
            particles = particles,
            rect = boundingBox particles,
            t = 0
        }
    where
        particles :: P.Parsec String () Particles 
        particles =  particle `P.sepBy` P.char '\n'

        particle :: P.Parsec String () Particle
        particle = do
            P.string "position="
            (x, y) <- pair
            P.string " velocity="
            (vx, vy) <- pair
            return Particle {x = x, y = y, vx = vx, vy = vy}

        pair :: P.Parsec String () (Int, Int)
        pair = (,) <$> (P.string "<" *> integer) <*> (P.string ", " *> integer <* P.string ">")

        integer :: P.Parsec String () Int
        integer = read <$> ((:) <$> P.choice [P.char ' ', P.char '-'] <*> P.many1 P.digit)

