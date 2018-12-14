module Input where
import Data.Text (Text, pack)
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text

data Timestamp = Timestamp {
    year :: Int,
    month :: Int,
    day :: Int,
    hour :: Int,
    minute :: Int
} deriving (Show,Eq,Ord)

data EntryType = EShift Int | Sleep | Wake
    deriving (Show,Eq)

data Entry = Entry {
    timestamp :: Timestamp,
    entryType :: EntryType
} deriving (Show)

parseAll :: String -> [Entry]
parseAll = map parseEntry . lines

parseEntry :: String -> Entry
parseEntry = unwrap . parse entryParser . pack

unwrap :: IResult Text Entry -> Entry
unwrap (Done _ line)    = line
unwrap (Partial f)      = unwrap $ f $ pack ""
unwrap (Fail _ _ err)   = error err

timestampParser = do
    char '['
    year <- decimal
    char '-'
    month <- decimal
    char '-'
    day <- decimal
    char ' '
    hour <- decimal
    char ':'
    minute <- decimal
    char ']'
    return $ Timestamp year month day hour minute

wakeParser = do
    string $ pack "wakes up"
    return Wake

sleepParser = do
    string $ pack "falls asleep"
    return Sleep

shiftParser = do
    string $ pack "Guard #"
    id <- decimal
    string $ pack " begins shift"
    return $ EShift id

entryTypeParser = choice [wakeParser, sleepParser, shiftParser]

entryParser = do
    ts <- timestampParser
    char ' '
    et <- entryTypeParser
    return $ Entry ts et
