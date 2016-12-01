import Text.Read (readMaybe)
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.IO (readFile)

data Direction = North | East | South | West deriving (Eq, Show)
data RelativeDirection = DirLeft | DirRight deriving (Eq, Show)
data MoveInstr = MoveInstr Direction Int deriving (Eq, Show)
data RelativeMove = RelativeMove RelativeDirection Int deriving (Eq, Show)
type Coordinate = (Int, Int)
data Location = Location Direction Coordinate [Coordinate] deriving (Eq, Show)

instance Enum Direction where
  fromEnum North = 0
  fromEnum East = 1
  fromEnum South = 2
  fromEnum West = 3
  toEnum 0 = North
  toEnum count = [North, East, South, West] !! (count `mod` 4)

updateCoord :: Coordinate -> MoveInstr -> Coordinate
updateCoord (x,y) (MoveInstr North amt) = (x, y + amt)
updateCoord (x,y) (MoveInstr South amt) = (x, y - amt)
updateCoord (x,y) (MoveInstr East amt)  = (x + amt, y)
updateCoord (x,y) (MoveInstr West amt)  = (x - amt, y)

updateDirection :: Direction -> RelativeDirection -> Direction
updateDirection curDir DirLeft = toEnum $ fromEnum curDir - 1
updateDirection curDir DirRight = toEnum $ fromEnum curDir + 1

parseInstruction :: String -> Either String RelativeMove
parseInstruction (dir:amt) =
  case toUpper dir of
    'R' -> parseInstruction' DirRight amt
    'L' -> parseInstruction' DirLeft amt
    _ -> Left "Invalid direction"
    where
      parseInstruction' :: RelativeDirection -> String -> Either String RelativeMove
      parseInstruction' rel s =
        let
          parseErr = Left "Unable to parse int"
          parsedInt = Right <$> (readMaybe s)
          instr' = fromMaybe parseErr parsedInt
        in (RelativeMove rel) <$> instr'

coordRange :: Coordinate -> Coordinate -> [Coordinate]
coordRange (x,y) (x',y') =
  if x == x' then
    [(x, y'') | y'' <- stepSeq y y']
  else
    [(x'', y) | x'' <- stepSeq x x']
  where
    stepSeq :: Int -> Int -> [Int]
    stepSeq a b = if a > b then [a,a-1..b] else [a..b]

updateLocation :: Location -> RelativeMove -> Location
updateLocation (Location dir spot hist) (RelativeMove moveDir amount) =
  let dir' = updateDirection dir moveDir
      newCardinal = updateDirection dir moveDir
      instr = MoveInstr newCardinal amount
      spot' = updateCoord spot instr
      hist' = hist ++ (tail $ coordRange spot spot')
   in Location dir' spot' hist'


move :: [String] -> Either String Location
move instructions =
  let start = Location North (0,0) []
      moves = sequence $ map parseInstruction instructions
  in foldl updateLocation start <$> moves

parseInstructions :: String -> [String]
parseInstructions = words . filter (/= ',')

absolutePostion :: Location -> Int
absolutePostion (Location _ coord _) = sumCoord coord

sumCoord :: Coordinate -> Int
sumCoord (x,y) = (abs x) + (abs y)

firstDoubleVisit :: [Coordinate] -> Maybe Coordinate
firstDoubleVisit [] = Nothing
firstDoubleVisit (first:rest) = if any (==first) rest then Just first else firstDoubleVisit rest

locHist :: Location -> [Coordinate]
locHist (Location _ current history) = history

main = do
  contents <- (head <$> getArgs) >>= readFile
  let moveResults = move (parseInstructions contents)
  putStr "Ending Location: " >> (print $ absolutePostion <$> moveResults)
  case firstDoubleVisit . locHist <$> moveResults of
    Left s -> print s
    Right (Just coords) -> putStr "first passed twice: " >> (print $ sumCoord coords)
    otherwise -> print "error"
