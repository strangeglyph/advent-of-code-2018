module Input where
import Data.Text (Text, pack)
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text

data Coord = Coord {
    x :: Int,
    y :: Int
} deriving (Show, Eq, Ord)

parseAll :: String -> [Coord]
parseAll = map parseCoord . lines


parseCoord :: String -> Coord
parseCoord = unwrap . parse coordParser . pack

unwrap :: IResult Text Coord -> Coord
unwrap (Done _ line)    = line
unwrap (Partial f)      = unwrap $ f $ pack ""
unwrap (Fail _ _ err)   = error err


coordParser = do
    x <- decimal
    string $ pack ", "
    y <- decimal
    return $ Coord x y
