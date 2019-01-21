module Year2018.Day03.Solution(solve) where
import qualified Data.List as List
import qualified Data.Matrix.Unboxed.Mutable as MUM
import qualified Data.Matrix.Unboxed as MU
import Text.Parsec
import Control.Monad

solve :: String -> [String]
solve input = [show $ part1 input, show $ part2 input]

data Claim = Claim { claimId :: Int
                   , claimX :: Int
                   , claimY :: Int
                   , claimWidth :: Int
                   , claimHeight :: Int
                   } deriving (Show)
claimMaxX c = claimX c + claimWidth c - 1
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
    claims = parseInput input

    maxX = (maximum $ map claimMaxX claims) ::Int
    maxY = (maximum $ map claimMaxY claims) ::Int

    mat :: MU.Matrix Int
    mat = MU.create $ do 
        m <- MUM.new (maxX + 1, maxY +1)
        forM_ claims $ \claim ->
            forM_ [claimX claim .. claimMaxX claim] $ \x -> 
                forM_ [claimY claim .. claimMaxY claim] $ \y -> do
                    v <- MUM.read m (x,y)
                    MUM.write m (x, y) (v + 1)
        return m

    res = sum $ do
        x <- [0 .. maxX]
        y <- [0 .. maxY]
        return $ if  mat MU.! (x,y) > 1 then 1 else 0
    in res

part2 :: String -> Int
part2 input = 42