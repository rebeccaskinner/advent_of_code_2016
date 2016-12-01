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
data Location = Location Direction Coordinate deriving (Eq, Show)

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

updateLocation :: Location -> RelativeMove -> Location
updateLocation (Location dir spot) (RelativeMove moveDir amount) =
  let dir' = updateDirection dir moveDir
      newCardinal = updateDirection dir moveDir
      instr = MoveInstr newCardinal amount
      spot' = updateCoord spot instr
   in Location dir' spot'


move :: [String] -> Either String Location
move instructions =
  let start = Location North (0,0)
      moves = sequence $ map parseInstruction instructions
  in foldl updateLocation start <$> moves

parseInstructions :: String -> [String]
parseInstructions = words . filter (/= ',')

absolutePostion :: Location -> Int
absolutePostion (Location _ (x,y)) = (abs x) + (abs y)

main = do
  contents <- (head <$> getArgs) >>= readFile
  let moveResults = move (parseInstructions contents)
  print $ absolutePostion <$> moveResults
