{-# LANGUAGE LambdaCase #-}
module Year2018.Day07.Solution(day07) where

import Solver
import Flow
import Text.Parsec
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Graph as G
import Control.Monad
import Control.Monad.Writer.Lazy
import Control.Monad.List
import Data.Maybe(mapMaybe, fromMaybe)
import Data.Char(ord)
import Data.List(sort)

day07 = Solver {
    name = "The Sum of Its Parts",
    year = 2018,
    day = 7,
    Solver.solve = \case
        Part1 -> Just . part1 
        Part2 -> Just . show . part2
}

type Job = Char 
type Time = Int
data Worker = Idle | Working Job Time deriving (Show, Eq, Ord)
newtype Graph = Graph (M.Map Job (S.Set Job)) deriving (Show)

empty :: Graph
empty = Graph M.empty 

nodes :: Graph -> [Job]
nodes (Graph m) = M.keys m

prerequisites :: Job -> Graph -> S.Set Job
prerequisites job (Graph m) =  fromMaybe S.empty (M.lookup job m)

removeNode :: Job -> Graph -> Graph
removeNode job (Graph m) =
    m 
    |> M.filterWithKey (\jobK _ -> jobK /= job)
    |> M.map (S.delete job)
    |> Graph

containsNode :: Job -> Graph -> Bool
containsNode job (Graph m) = M.member job m

addNode :: Job -> Graph -> Graph
addNode job g@(Graph m) = 
    if containsNode job g
        then g 
        else Graph $ M.insert job S.empty m

addEdge :: Job -> Job -> Graph -> Graph
addEdge jobA jobB g@(Graph m) = 
    g 
    |> addNode jobA 
    |> addNode jobB
    |> (\(Graph m) -> let 
            edges = M.findWithDefault S.empty jobA m
            edges' = S.insert jobB edges
            m' = M.insert jobA edges' m
        in Graph m')

isEmpty :: Graph -> Bool
isEmpty (Graph m) = M.null m

topSort :: Graph -> [Job]
topSort g@(Graph m) 
    | isEmpty g = []
    | otherwise = 
        let
            nextJob = m 
                |> M.filter null    -- jobs that doesn't have unsatisfied dependencies
                |> M.findMin        -- take the first one alphabetically
                |> fst              -- get the key

        in (nextJob : topSort (removeNode nextJob g))
        
part1 :: String -> String
part1 input = let
    g = parseInput input
    jobs = topSort g
    in jobs


part2 :: String -> Time
part2 input = let

    time :: Job -> Time
    time job = 60 + ord job - ord 'A'

    removeFinishedJobs :: [Worker] -> Graph -> Graph
    removeFinishedJobs workers g = 
        let
            finishedJobs = workers |> mapMaybe (\case (Working j 0) -> Just j; _ -> Nothing)
        in
            foldr removeNode g finishedJobs
    
    availableJobs :: Graph -> [Worker]-> [Job]
    availableJobs g workers = 
        let
            inProgress = workers |> mapMaybe (\case (Working j _) -> Just j; _ -> Nothing) |> S.fromList
            noPrerequisites = g |> nodes |> filter (\node -> null $ prerequisites node g) |> S.fromList
            available =  S.difference noPrerequisites inProgress
        in 
            S.toList available |> sort

    stepWorkers :: [Worker] -> [Job] -> [Worker]
    stepWorkers [] jobs = []
    stepWorkers (worker: ws) jobs = 
        let
            (worker', jobs') = stepWorker (worker, jobs)
        in 
            worker' : stepWorkers ws jobs'

    stepWorker :: (Worker, [Job]) -> (Worker, [Job])
    stepWorker (worker, jobs) =
        case worker of
            Working _ 0 -> pickJob jobs
            Working j t -> (Working j (t - 1), jobs)
            Idle -> pickJob jobs
            where 
                pickJob [] = (Idle, [])
                pickJob (job:jobs) = (Working job (time job), jobs)

    runSimulation :: Graph -> [Worker] -> Time -> Time
    runSimulation g workers i
        | isEmpty g && all (\case Idle -> True; _ -> False) workers  = i - 1
        | otherwise = 
            let 
                g' = removeFinishedJobs workers g
                workers' = stepWorkers workers (availableJobs g' workers)
            in 
                runSimulation g' workers' (i + 1)
    
    in runSimulation (parseInput input) [Idle, Idle, Idle, Idle, Idle] 0

parseInput :: String -> Graph
parseInput input =
    case parse dependencies "" input of
        Left e -> error $ show e
        Right dependencies -> toGraph dependencies
    where
        dependencies :: Parsec String () [(Job, Job)]
        dependencies =  dependency `sepBy` char '\n'

        dependency :: Parsec String () (Job, Job)
        dependency = do
            string "Step "
            b <- letter
            string " must be finished before step "
            a <- letter
            string " can begin."
            return  (a, b)

        toGraph ::  [(Job, Job)] -> Graph
        toGraph dependencies = 
            let
                g = foldr addNode empty $ fst <$> dependencies
                g' = foldr addNode g $ snd <$> dependencies
                g'' = foldr (\(jobA, jobB) gT -> addEdge jobA jobB gT) g' dependencies
            in g''