import Data.Maybe
import System.Environment
import Data.List
import Data.List.Split
import Text.Read

blankChar = ' '
filledChar = '#'

data Instruction = Rect Int Int | RotateRow Int Int | RotateCol Int Int deriving (Eq, Show)

applyInstruction :: Instruction -> [String] -> [String]
applyInstruction (Rect x y) = setMatrixRegion x y filledChar
applyInstruction (RotateRow rowID amount) = rotateRow amount rowID
applyInstruction (RotateCol colID amount) = rotateCol amount colID

parseInstruction :: String -> Maybe Instruction
parseInstruction input =
  case words input of
    ("rect":rest) -> uncurry Rect <$> parseXY rest
    ("rotate":"row":rest) -> uncurry RotateRow <$> parseIDAmount rest
    ("rotate":"column":rest) -> uncurry RotateCol <$> parseIDAmount rest
    _ -> Nothing
  where
    parseXY :: [String] -> Maybe (Int, Int)
    parseXY fields =
      case (wordsBy (=='x') . head) fields of
        [x,y] -> (,) <$> readMaybe x <*> readMaybe y
        _ -> Nothing
    parseIDAmount :: [String] -> Maybe (Int, Int)
    parseIDAmount fields =
      let amnt = (readMaybe . last) fields
          id = (readMaybe . last . wordsBy (== '=') . head) fields
      in (,) <$> id <*> amnt

parseInput :: String -> Maybe [Instruction]
parseInput = mapM parseInstruction . lines

runInstructions :: [String] -> [Instruction] -> [String]
runInstructions = foldl (flip applyInstruction)

rotateList' :: [a] -> [a]
rotateList' l = last l : init l

rotateList = foldl1 (.) . (`replicate` rotateList')

getColumn :: Int -> [[a]] -> [a]
getColumn idx lst =
    let lst' = transpose lst
        idx' = idx `mod` length lst' in
  lst' !! idx'

setField :: Int -> [a] -> a -> [a]
setField idx lst val =
  let (front, back) = splitAt idx lst in
    front ++ (val : tail back)

setSpan :: Int -> Int -> [a] -> a -> [a]
setSpan start end lst val =
  let instrs = foldl (\carry cur -> (\x -> setField cur x val ) . carry) (const lst) [start..end - 1] in
    instrs lst

setColumn :: Int -> [[a]] -> [a] -> [[a]]
setColumn idx lst newcol =
  let lst' = transpose lst in
    transpose $ setField idx lst' newcol

rotateCol' :: Int -> [[a]] -> [[a]]
rotateCol' idx lst =
  setColumn idx lst $ rotateList' (getColumn idx lst)

rotateCol :: Int -> Int -> [[a]] -> [[a]]
rotateCol amnt col = foldl1 (.) (replicate amnt (rotateCol' col))

rotateRow' :: Int ->[[a]] -> [[a]]
rotateRow' idx lst = setField idx lst (rotateList' $ lst !! idx)

rotateRow :: Int -> Int -> [[a]] -> [[a]]
rotateRow amnt row = foldl1 (.) (replicate amnt (rotateRow' row))

setMatrixRegion :: Int -> Int -> a -> [[a]] -> [[a]]
setMatrixRegion x y val lst =
  let newRows = zip [0..] $ take y $ map (\row -> setSpan 0 x row val) lst in
    foldl (\lst' (idx, row) -> setField idx lst' row) lst newRows

commandMtx :: Int -> Int -> String -> Maybe [String]
commandMtx x y commands =
  let instructions = parseInput commands
      startingMtx = replicate y (replicate x blankChar)
  in runInstructions startingMtx <$> instructions

run :: String -> IO ()
run cmds =
  let mtx = commandMtx 50 6 cmds
      count = (length . filter (==filledChar) . concat) <$> mtx
  in do
    mapM_ print $ fromJust mtx
    print count

main = do
  input <- (head <$> getArgs) >>= readFile
  run input
