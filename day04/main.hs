import Data.List
import Data.Maybe
import Data.Ord
import GHC.Exts
import Input
import Utils

main = do
    input <- readFile "input"
    putStrLn $ "a: " ++ (solveA input)
    putStrLn $ "b: " ++ (solveB input)

solveA :: String -> String
solveA inpt = show (tgtGuard * tgtMinute)
    where   tgtGuard = guardWithMostSleep $ shiftsByGuard $ shifts inpt
            tgtMinute = minuteMostAsleep $ findGuardShifts (shifts inpt) tgtGuard

solveB :: String -> String
solveB inpt = show $ tgtGuard * tgtMinute
    where   groupedShifts = shiftsByGuard $ shifts inpt
            minutesMostAsleep = map minuteMostAsleep groupedShifts
            counts = map length $ zipWith intervalsAsleep groupedShifts minutesMostAsleep
            maxCount = maximum counts :: Int
            maxIdx = fromJust $ findIndex (== maxCount) counts
            tgtGuard = guardId $ head $ groupedShifts !! maxIdx
            tgtMinute = minutesMostAsleep !! maxIdx

shiftsByGuard :: [Shift] -> [[Shift]]
shiftsByGuard = groupWith guardId

shifts :: String -> [Shift]
shifts = map entriesToShift . groupShifts . parseAll

groupShifts :: [Entry] -> [[Entry]]
groupShifts = groupBy (\_ b -> isShift b)
    where isShift = flip elem [Wake, Sleep] . entryType

entriesToShift :: [Entry] -> Shift
entriesToShift ((Entry _ (EShift gid)):xs) = Shift gid intervals
    where intervals = zipWith entryToInterval (odds xs) (evens xs)

entryToInterval :: Entry -> Entry -> Interval
entryToInterval (Entry ts1 Sleep) (Entry ts2 Wake)
    | day ts1 == day ts2    = Interval (hour ts1) (minute ts1) (hour ts2) (minute ts2)
    | otherwise             = error "Cross day interval"

intervalLength :: Interval -> Int
intervalLength (Interval startHour startMinute endHour endMinute)
    = endMinute - startMinute + 60 * (endHour - startHour)

intervalContainsMin :: Int -> Interval -> Bool
intervalContainsMin min (Interval startHour startMinute endHour endMinute)
    | startHour == endHour      = startMinute <= min && min < endMinute
    | startHour == endHour - 1  = startMinute <= min || min < endMinute
    | otherwise                 = True

sleepsDuringShift :: Shift -> Int
sleepsDuringShift = sum . map intervalLength . sleepTimes

cumulativeSleep :: [Shift] -> (Int, Int)
cumulativeSleep shifts = (guardId $ head shifts, sum $ map sleepsDuringShift shifts)

guardWithMostSleep :: [[Shift]] -> Int
guardWithMostSleep = fst . last . sortBy (comparing snd) . map cumulativeSleep

minuteMostAsleep :: [Shift] -> Int
minuteMostAsleep shifts = fromJust $ elemIndex maxElem intPerMin
    where   intPerMin = map (intervalsAsleep shifts) [0..59]
            maxElem = maximumBy (comparing length) intPerMin

intervalsAsleep :: [Shift] -> Int -> [Interval]
intervalsAsleep shifts min = filter (intervalContainsMin min) $ concatMap sleepTimes shifts

findGuardShifts :: [Shift] -> Int -> [Shift]
findGuardShifts xs gid = filter ((==) gid . guardId) xs


data Interval = Interval {
    startHour :: Int,
    startMinute :: Int,
    endHour :: Int,
    endMinute :: Int
} deriving (Show,Eq)

data Shift = Shift {
    guardId :: Int,
    sleepTimes :: [Interval]
} deriving (Show,Eq)
