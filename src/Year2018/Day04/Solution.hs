module Year2018.Day04.Solution(solve) where

import qualified Data.Matrix as M
import qualified Data.List as L

import Text.Parsec
import Control.Monad

import Debug.Trace
solve :: String -> [String]
solve input = [show $ part1 input, show $ part2 input]

data DateTime = DateTime { dateTimeYear :: Int
                         , dateTimeMonth :: Int
                         , dateTimeDay :: Int
                         , dateTimeHour :: Int
                         , dateTimeMin :: Int
                         } deriving (Show, Eq, Ord)

data Day = Day { dayGuardId :: Int
               , daySleep :: [Int]
               } deriving (Show)

data TimeTable = TimeTable {
    ttGuardId :: Int,
    ttSleep :: M.Matrix Int
} deriving (Show)

totalSleep :: TimeTable -> Int
totalSleep tt = sum $ M.flatten $ ttSleep tt

sleepByMinute :: TimeTable -> [(Int,Int)]
sleepByMinute tt = map (\t -> (t, sum $ M.takeColumn (ttSleep tt) t)) [0..59]

maxSleepByMinute :: TimeTable -> (Int,Int)
maxSleepByMinute tt = L.maximumBy (\a b -> compare (snd a) (snd b)) $ sleepByMinute tt

parseInput :: String -> [TimeTable]
parseInput input = 

    let sortedLines = L.intercalate "\n" $ L.sort $ lines input
    in 
        case parse days "" sortedLines of
            Left e -> error $ show e
            Right days -> timeTables days

    where
        days :: Parsec String () [Day]
        days = many day

        day :: Parsec String () Day
        day = do
            guardId <- dateTime *> string " Guard #" *> integer <* string " begins shift\n"
            sleeps <- many $ try sleep
            return $ Day guardId (mask sleeps)

        sleep :: Parsec String () (Int, Int)
        sleep = do
            fallsAsleep <- dateTime <* string " falls asleep\n"
            wakesUp <- dateTime <* string " wakes up" <* optional (char '\n')
            return (dateTimeMin fallsAsleep, dateTimeMin wakesUp)
          
        dateTime :: Parsec String () DateTime
        dateTime = DateTime <$> 
                (string "[" *> integer) <*>
                (string "-" *> integer) <*>
                (string "-" *> integer) <*>
                (string " " *> integer) <*>
                (string ":" *> integer) <*
                string "]"
        
         
        integer :: Parsec String () Int
        integer = read <$> many digit
        
        mask :: [(Int, Int)] -> [Int]
        mask sleeps = 
            do 
                t <- [0..59]
                return $ if any (\(fallsAsleep, wakesUp) -> fallsAsleep <= t && t < wakesUp) sleeps then 1 else 0

        timeTables :: [Day] -> [TimeTable]
        timeTables days = 
            do 
                group <- L.groupBy (\dayA dayB -> dayGuardId dayA == dayGuardId dayB) $ 
                         L.sortBy (\dayA dayB -> compare (dayGuardId dayA) (dayGuardId dayB)) days
                let ttGuardId = dayGuardId $ head group
                let ttSleep = M.fromLists $ map daySleep group
                return $ TimeTable ttGuardId ttSleep

part1 :: String -> Int
part1 input = 
    let 
        timeTables = parseInput input
        tt = L.maximumBy (\ttA ttB -> compare (totalSleep ttA) (totalSleep ttB)) timeTables
        minute = fst $ L.maximumBy (\a b -> compare (snd a) (snd b)) (sleepByMinute tt)
    in ttGuardId tt * minute

part2 :: String -> Int
part2 input =
    let 
        timeTables = parseInput input
        tt = L.maximumBy (\ttA ttB -> compare (snd $ maxSleepByMinute ttA) (snd $ maxSleepByMinute ttB)) timeTables
        minute = fst $ maxSleepByMinute tt
    in ttGuardId tt * minute
