module Year2018.Day03.Solution(day03) where
import Solver

import qualified Data.Matrix.Unboxed.Mutable as MUM
import qualified Data.Matrix.Unboxed as MU

import Text.Parsec
import Control.Monad

day03 = Solver {
    name = "No Matter How You Slice It",
    year = 2018,
    day = 3,
    solve = \input -> show <$> [part1 input, part2 input]
}

data Claim = Claim { claimId :: Int
                   , claimX :: Int
                   , claimY :: Int
                   , claimWidth :: Int
                   , claimHeight :: Int
                   } deriving (Show)

claimMaxX :: Claim -> Int
claimMaxX c = claimX c + claimWidth c - 1

claimMaxY :: Claim -> Int
claimMaxY c = claimY c + claimHeight c - 1

integer :: Parsec String () Int
integer = read <$> many digit

claim :: Parsec String () Claim
claim = Claim <$> 
    (string "#"   *> integer) <*>
    (string " @ " *> integer) <*>
    (string ","   *> integer) <*>
    (string ": "  *> integer) <*>
    (string "x"   *> integer)

claims :: Parsec String () [Claim]
claims = many (claim <* optional (char '\n'))

parseInput :: String -> [Claim]
parseInput input = 
    case parse claims "" input of
        Left e -> error $ show e
        Right claims -> claims

part1 :: String -> Int
part1 input = let
    claims :: [Claim]
    claims = parseInput input

    maxX :: Int
    maxX = (maximum $ map claimMaxX claims)
    
    maxY :: Int
    maxY = (maximum $ map claimMaxY claims)

    mat :: MU.Matrix Int
    mat = MU.create $ do 
        m <- MUM.new (maxX + 1, maxY +1)
        forM_ claims $ \claim ->
            forM_ [claimX claim .. claimMaxX claim] $ \x -> 
                forM_ [claimY claim .. claimMaxY claim] $ \y -> do
                    v <- MUM.read m (x,y)
                    MUM.write m (x, y) (v + 1)
        return m

    in sum $ do
        x <- [0 .. maxX]
        y <- [0 .. maxY]
        return $ if  mat MU.! (x,y) > 1 then 1 else 0

part2 :: String -> Int
part2 input = let
    claims :: [Claim]
    claims = parseInput input

    intact :: Claim -> Bool
    intact claim = all (not . overlap claim) claims

    overlap :: Claim -> Claim -> Bool
    overlap claimA claimB = 
        (claimId claimA /= claimId claimB) &&
        claimX claimA <= claimMaxX claimB && claimX claimB <= claimMaxX claimA &&
        claimY claimA <= claimMaxY claimB && claimY claimB <= claimMaxY claimA

    in claimId $ head $ filter intact claims