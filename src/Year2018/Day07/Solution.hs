module Year2018.Day07.Solution(day07) where
import Solver
import Flow
import Text.Parsec
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace

day07 = Solver {
    name = "The Sum of Its Parts",
    year = 2018,
    day = 7,
    Solver.solve = \case
        Part1 -> Just . part1 
        Part2 -> Just . show . part2
}

newtype Step = Step Char deriving (Show, Eq, Ord)
newtype Graph = Graph (M.Map Step (S.Set Step)) deriving (Show)
empty :: Graph
empty = Graph M.empty 

removeNode :: Step -> Graph -> Graph
removeNode step (Graph m) =
    m 
    |> M.filterWithKey (\stepK _ -> stepK /= step)
    |> M.map (S.delete step)
    |> Graph

containsNode :: Step -> Graph -> Bool
containsNode step (Graph m) = M.member step m

addNode :: Step -> Graph -> Graph
addNode step g@(Graph m) = 
    if containsNode step g
        then g 
        else Graph $ M.insert step S.empty m

addEdge :: Step -> Step -> Graph -> Graph
addEdge stepA stepB g@(Graph m) = 
    g 
    |> addNode stepA 
    |> addNode stepB
    |> (\(Graph m) -> let 
            edges = M.findWithDefault S.empty stepA m
            edges' = S.insert stepB edges
            m' = M.insert stepA edges' m
        in Graph m')

isEmpty :: Graph -> Bool
isEmpty (Graph m) = M.null m

topSort :: Graph -> [Step]
topSort g@(Graph m) 
    | isEmpty g = []
    | otherwise = 
        let
            nextStep = m 
                |> M.filter null    -- steps that doesn't have unsatisfied dependencies
                |> M.findMin        -- take the first one alphabetically
                |> fst              -- get the key

        in (nextStep : topSort (removeNode nextStep g))

part1 :: String -> String
part1 input = let
    g = parseInput input
    in  (\(Step s)-> s) <$> topSort g

part2 :: String -> Int
part2 input = 42

parseInput :: String -> Graph
parseInput input =
    case parse dependencies "" input of
        Left e -> error $ show e
        Right dependencies -> toGraph dependencies
    where
        dependencies :: Parsec String () [(Step, Step)]
        dependencies =  dependency `sepBy` char '\n'

        dependency :: Parsec String () (Step, Step)
        dependency = do
            string "Step "
            b <- letter
            string " must be finished before step "
            a <- letter
            string " can begin."
            return (Step a, Step b)

        toGraph ::  [(Step, Step)] -> Graph
        toGraph dependencies = 
            let
                g = foldr addNode empty $ fst <$> dependencies
                g' = foldr addNode g $ snd <$> dependencies
                g'' = foldr (\(stepA, stepB) gT -> addEdge stepA stepB gT) g' dependencies
            in g''