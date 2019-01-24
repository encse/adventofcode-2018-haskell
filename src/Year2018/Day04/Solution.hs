module Year2018.Day04.Solution(solve) where

import Data.List
import Data.Function
import Text.Parsec

import qualified Data.Matrix as M

solve :: String -> [String]
solve input = [show $ part1 input, show $ part2 input]

data DateTime = DateTime { dateTimeYear :: Int
                         , dateTimeMonth :: Int
                         , dateTimeDay :: Int
                         , dateTimeHour :: Int
                         , dateTimeMinute :: Int
                         } deriving (Show, Eq, Ord)

data Day = Day { dayGuardId :: Int
               , daySleep :: [Int]
               } deriving (Show)

data TimeTable = TimeTable {
    ttGuardId :: Int,
    ttSleep :: M.Matrix Int
} deriving (Show)

totalSleep :: TimeTable -> Int
totalSleep tt = M.foldl (+) 0 $ ttSleep tt

sleepByMinute :: TimeTable -> [(Int,Int)]
sleepByMinute tt = (\t -> (t, sum $ M.takeColumn (ttSleep tt) t)) <$> [0..59]

maxSleepByMinute :: TimeTable -> (Int,Int)
maxSleepByMinute tt = maximumBy (compare `on` snd) $ sleepByMinute tt

parseInput :: String -> [TimeTable]
parseInput input = 

    let sortedLines = intercalate "\n" $ sort $ lines input
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
            return (dateTimeMinute fallsAsleep, dateTimeMinute wakesUp)
          
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
        mask sleepIntervals = fromEnum . sleepsAt <$> [0..59]
            where 
                sleepsAt :: Int -> Bool
                sleepsAt t = any (contains t) sleepIntervals
                
                contains :: Int -> (Int, Int) -> Bool
                contains t (fallsAsleep, wakesUp) = fallsAsleep <= t && t < wakesUp

        timeTables :: [Day] -> [TimeTable]
        timeTables days = 
            do 
                daysGroupedByGuard <- sortBy (compare `on` dayGuardId) days & 
                                      groupBy ((==) `on` dayGuardId)

                let ttGuardId = dayGuardId $ head daysGroupedByGuard
                let ttSleep = M.fromLists $ map daySleep daysGroupedByGuard
                return $ TimeTable ttGuardId ttSleep

part1 :: String -> Int
part1 input = 
    let 
        timeTables = parseInput input
        tt = maximumBy (compare `on` totalSleep) timeTables
        minute = fst $ maximumBy (compare `on` snd) (sleepByMinute tt)
    in ttGuardId tt * minute

part2 :: String -> Int
part2 input =
    let 
        timeTables = parseInput input
        tt = maximumBy (compare `on` snd . maxSleepByMinute) timeTables
        minute = fst $ maxSleepByMinute tt
    in ttGuardId tt * minute