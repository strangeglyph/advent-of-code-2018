import Data.Array
import Data.Text (Text, pack)
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text
import Debug.Trace

main = do
    input <- readFile "input"
    putStrLn $ solveA input
    putStrLn $ solveB input

solveA :: String -> String
solveA input = show $ length $ filter (>1) $ elems counted
    where lines' = map parseLine $ lines input
          fld = field lines'
          counted = foldl increment fld $ Prelude.take 1 lines'

solveB :: String -> String
solveB = undefined




field :: [Line] -> Array (Int,Int) Int
field ls = array (boundsLs ls) [((i,j), 0) | i <- [0..extentX ls], j <- [0..extentY ls]]


boundsLs :: [Line] -> ((Int, Int), (Int, Int))
boundsLs ls = ((0, 0), (extentX ls, extentY ls))

extentX, extentY :: [Line] -> Int
extentX = maximum . map rightBorder
extentY = maximum . map bottomBorder


parseLine :: String -> Line
parseLine = unwrap . parse lineRule . pack

unwrap :: IResult Text Line -> Line
unwrap (Done _ line)    = line
unwrap (Partial f)      = unwrap $ f $ pack ""
unwrap (Fail _ _ err)   = error err

lineRule :: Parser Line
lineRule = do
    char '#'
    id <- decimal
    string $ pack " @ "
    offsetX <- decimal
    char ','
    offsetY <- decimal
    string $ pack ": "
    width <- decimal
    char 'x'
    height <- decimal
    return $ Line id offsetX offsetY width height

increment :: Array (Int,Int) Int -> Line -> Array (Int,Int) Int
increment arr line = trace (show $ Main.id line) $! foldr incrementOne arr $ contained line

incrementOne :: (Int, Int) -> Array (Int,Int) Int -> Array (Int,Int) Int
incrementOne idx arr = trace (show idx) $! arr//[(idx,arr!idx)]

contained :: Line -> [(Int, Int)]
contained line = [(x,y) | x <- [loX..hiX], y <- [loY..hiY]]
    where loX = offsetX line
          hiX = rightBorder line - 1
          loY = offsetY line
          hiY = bottomBorder line - 1



rightBorder :: Line -> Int
rightBorder l = width l + offsetX l

bottomBorder :: Line -> Int
bottomBorder l = height l + offsetY l

data Line = Line {
    id :: Int,
    offsetX :: Int,
    offsetY :: Int,
    width :: Int,
    height :: Int
} deriving (Show)
