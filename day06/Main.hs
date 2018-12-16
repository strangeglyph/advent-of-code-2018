import Control.Monad
import Data.List
import Data.Maybe
import Input

main = do
    input <- readFile "input"
    putStrLn $ "a: " ++ (solveA input)
    putStrLn $ "b: " ++ (solveB input)

solveA :: String -> String
solveA inpt = show $ maximum $ map length $ group $ sort $ filter (not . flip elem bIds) $ fld
    where coords = parseAll inpt
          fld = field coords
          bIds = borderIds coords

solveB :: String -> String
solveB = show . regionSize . parseAll


isInfinite :: [Coord] -> Coord -> Bool
isInfinite cs c = x c == minX cs || x c == maxX cs || y c == minY cs || y c == maxY cs

borderIds :: [Coord] -> [Coord]
borderIds cs = nub $ catMaybes $ map (closest cs) $ nub  $ horizontal ++ vertical
    where horizontal = liftM2 Coord [0..maxX cs] [0, maxY cs]
          vertical = liftM2 Coord [0, maxX cs] [0..maxY cs]

field :: [Coord] -> [Coord]
field cs = catMaybes $ map (closest cs) $ liftM2 Coord [0..maxX cs] [0..maxY cs]


closest :: [Coord] -> Coord -> Maybe Coord
closest coords p = case closestCoords of
        [x] -> Just x
        _   -> Nothing
    where minDist = minimum $ map (manhattan p) coords
          closestCoords = filter (\x -> manhattan p x == minDist) coords


regionSize :: [Coord] -> Int
regionSize cs = length $ filter (<10000) $ map (cumDist cs) $ liftM2 Coord [0..maxX cs] [0..maxY cs]


cumDist :: [Coord] -> Coord -> Int
cumDist coords p = sum $ map (manhattan p) coords

manhattan :: Coord -> Coord -> Int
manhattan p1 p2 = abs (x p1 - x p2) + abs (y p1 - y p2)

minX = minimum . map x
minY = minimum . map y
maxX = maximum . map x
maxY = maximum . map y
