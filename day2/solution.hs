import Data.Char (toUpper)
import System.IO (readFile)
import System.Environment (getArgs)

data Instruction = U | D | L | R deriving (Eq, Show)
type Coord = (Int, Int) -- (x,y)

updateCoord :: Instruction -> Coord -> Coord
updateCoord U (x,y) = (x, y + 1)
updateCoord D (x,y) = (x, y - 1)
updateCoord L (x,y) = (x - 1, y)
updateCoord R (x,y) = (x + 1, y)

parseInstr :: Char -> Either String Instruction
parseInstr 'U' = Right U
parseInstr 'D' = Right D
parseInstr 'L' = Right L
parseInstr 'R' = Right R
parseInstr badInstr = Left (badInstr : ": invalid instruction")

parseFile :: String -> Either String [[Instruction]]
parseFile = mapM (mapM parseInstr) . lines

move :: Coord -> Instruction -> Coord
move coord instr =
  let coord' = updateCoord instr coord in
    if inRange coord' then
      coord'
    else
      coord

moveRow :: Coord -> [Instruction] -> Coord
moveRow = foldl move

buttonCoords :: Coord -> [[Instruction]] -> [Coord]
buttonCoords = (tail .) . scanl moveRow

buttons :: Coord -> [[Instruction]] -> [Int]
buttons = (map toNum .) . buttonCoords

inRange :: Coord -> Bool
inRange (x,y) = (x `elem` [0..2]) && (y `elem` [0..2])

toNum :: Coord -> Int
toNum (x,y) = x + (y * 3) + 1

toCoord :: Int -> Coord
toCoord idx =
  let idx' = idx - 1
      row  = idx' `div` 3
      col  = idx' `mod` 3
  in (col, row)

main = do
  instr <- (head <$> getArgs) >>= readFile
  print $ concatMap show <$> buttons (1,1) <$> parseFile instr
