import Data.Char (toUpper)
import System.IO (readFile)
import System.Environment (getArgs)

data Instruction = U | D | L | R deriving (Eq, Show)
type Coordinate = (Int, Int) -- (x,y)
type Keypad = (Int, Int) -- (rows, cols)

bindCoordinates :: Coordinate -> Keypad -> Coordinate
bindCoordinates (x,y) (rows,cols) = (within cols x, within rows y)
  where within a b = max 0 (min a b)

instructionTransform :: Instruction -> Coordinate -> Coordinate
instructionTransform U (x,y) = (x, y - 1)
instructionTransform D (x,y) = (x, y + 1)
instructionTransform L (x,y) = (x - 1, y)
instructionTransform R (x,y) = (x + 1, y)

bindTransform :: Keypad -> Instruction -> Coordinate -> Coordinate
bindTransform a b c = (instructionTransform b c) `bindCoordinates` a

parseInstructionRow :: String -> Either String [Instruction]
parseInstructionRow = sequence . map (toInstr . toUpper)
  where
    toInstr 'U' = Right U
    toInstr 'D' = Right D
    toInstr 'L' = Right L
    toInstr 'R' = Right R
    toInstr instr = Left (instr : " is not a valid instruction")

applyInstructions :: Keypad -> Coordinate -> [Instruction] -> Coordinate
applyInstructions k c instrs =
  let update = moveCoord k in
    foldl (flip update) c instrs

coordsToNum :: Keypad -> Coordinate -> Int
coordsToNum k@(rows,cols) c =
  let (x,y) = bindCoordinates c (rows-1, cols-1) in (y * cols) + x + 1

move :: Int -> Instruction -> Int
move current act =
  coordsToNum (3,3) $ bindTransform (3,3) act (tupleFromIndex current)

moveCoord :: Keypad -> Instruction -> Coordinate -> Coordinate
moveCoord k i c = tupleFromIndex $ move (coordsToNum k c) i

tupleFromIndex x =
  let whichCol = (x - 1) `div` 3
      whichRow = x - (whichCol * 3)
  in (whichRow - 1, whichCol)

main = do
  instructions <- lines <$> (head <$> getArgs >>= readFile)
  let parsed = mapM parseInstructionRow instructions
  let results = map (coordsToNum (3,3)) . tail <$> scanl (applyInstructions (3,3)) (1,1) <$> parsed
  print $ concatMap show <$> results
