{-# LANGUAGE TupleSections #-}
module Year2018.Day09.Solution(day09) where

import Solver
import Flow
import Text.Parsec
import qualified Data.Map as M
import Data.Maybe(fromJust)
import Debug.Trace

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
newtype Ref = Ref Int deriving (Show, Eq, Ord)

data LinkedListItem = LinkedListItem {
    lliPrevId :: Ref,
    lliNextId :: Ref,
    lliItem :: Int
} deriving (Show)

data LinkedList = LinkedList {
    llCurrent:: LinkedListItem,
    llNextId :: Ref,
    llMap :: M.Map Ref LinkedListItem
} 

instance Show Point where
    show (Point p) = show p

llCurrentId :: LinkedList -> Ref
llCurrentId ll = prev ll |> llCurrent |> lliNextId

singleton :: Int -> LinkedList
singleton i = let
        id = Ref 0
        nextId = Ref 1
        item = i
        lliNew = LinkedListItem id id item
        map = M.singleton id lliNew
    in
        LinkedList lliNew nextId map


next :: LinkedList -> LinkedList
next (LinkedList lli nextId map) = let
        lliT = fromJust $ M.lookup (lli |> lliNextId) map 
   in 
        LinkedList lliT nextId map

prev :: LinkedList -> LinkedList
prev (LinkedList lli nextId map) = let
        lliT = fromJust $ M.lookup (lli |> lliPrevId) map 
    in
        LinkedList lliT nextId map

insert :: Int -> LinkedList -> LinkedList
insert i ll = let

        prevId = ll |> llCurrentId
        nextId = ll |> llCurrent |> lliNextId

        Ref newId = ll |> llNextId
        newNextId = Ref (newId + 1)
        newItem = i

        lliNew = LinkedListItem prevId nextId newItem
       
        update id l map = M.insert id (l (fromJust $ M.lookup id map)) map
            
        map = ll |> llMap 
            |> M.insert (Ref newId) lliNew
            |> M.adjust (\ lli -> LinkedListItem (lli |> lliPrevId) (Ref newId) (lli |> lliItem)) prevId
            |> M.adjust (\ lli -> LinkedListItem (Ref newId) (lli |> lliNextId) (lli |> lliItem)) nextId
    in
        LinkedList lliNew newNextId map


remove :: LinkedList -> LinkedList
remove ll = let
        prevId = ll |> llCurrent |> lliPrevId
        nextId = ll |> llCurrent |> lliNextId
        currentId = ll |> llCurrentId

        lli = fromJust $ M.lookup nextId map 

        map = ll |> llMap 
            |> M.delete currentId
            |> M.adjust (\ lli -> LinkedListItem (lli |> lliPrevId) nextId (lli |> lliItem)) prevId
            |> M.adjust (\ lli -> LinkedListItem prevId (lli |> lliNextId) (lli |> lliItem)) nextId
    in
        LinkedList lli (ll |> llNextId) map


items :: LinkedList -> [Int]
items ll =  let
        itemsI 0 ll = []
        itemsI n ll = (ll |> llCurrent |> lliItem) : itemsI (n - 1) (ll |> next)
    in itemsI (M.size $ llMap ll) ll

tsto :: LinkedList -> String
tsto ll = show $ items ll

run :: String -> Int -> Point
run input mul = let

    (playerCount, marbleCount) = parseInput input
    lastMarble = marbleCount * mul
    
    players = cycle (PlayerId <$> [1..playerCount])
    marbles = zip players (Marble <$> [1 .. lastMarble]) 

    initialScores :: M.Map PlayerId Point
    initialScores = M.fromList $ (\id -> (PlayerId id, Point 0)) <$> [1 .. playerCount]

    ll = singleton 0

    step :: (M.Map PlayerId Point, LinkedList) -> (PlayerId, Marble) -> (M.Map PlayerId Point, LinkedList) 
    step (scores, ll) (player, Marble marble) 
        | marble `mod` 23 == 0 = let
                x = ll |> prev |> prev |> prev |> prev |> prev |> prev |> prev 
                points = toInteger $ marble + (x |> llCurrent |> lliItem)
                newLinkedList = x |> remove
                newScores = M.adjust (\(Point p) -> Point $ p + points) player scores
            in 
                (newScores, newLinkedList)

        | otherwise = let 
                llT = ll |> next |> insert marble
            in 
               (scores, llT)

    (scores, _) = foldl step (initialScores, ll) marbles
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
