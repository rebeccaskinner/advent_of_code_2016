import System.IO (readFile)
import System.Environment (getArgs)
import Data.Maybe (isNothing, isJust, fromJust)

type Button = Maybe Char
data Move = U|D|L|R deriving (Eq, Show)
type Coord = (Int, Int)

parseMove :: Char -> Either String Move
parseMove 'U' = Right U
parseMove 'D' = Right D
parseMove 'L' = Right L
parseMove 'R' = Right R
parseMove instr = Left (instr : " is not a valid instruction")

parseMoveList :: String -> Either String [Move]
parseMoveList = mapM parseMove

parseCode :: String -> Either String [[Move]]
parseCode = mapM parseMoveList . lines

grid :: [[Button]]
grid = [[Nothing, Nothing, Just '1', Nothing, Nothing]
       ,[Nothing, Just '2', Just '3', Just '4', Nothing]
       ,[Just '5', Just '6', Just '7', Just '8', Just '9']
       ,[Nothing, Just 'A', Just 'B', Just 'C', Nothing]
       ,[Nothing, Nothing, Just 'D', Nothing, Nothing]]

move :: Coord -> Move -> Coord
move pos instr =
  let pos' = updatePos instr pos in
    if isValid pos' then pos' else pos

isValid = isJust . buttonAt

updatePos :: Move -> Coord -> Coord
updatePos U (x,y) = (x, y - 1)
updatePos D (x,y) = (x, y + 1)
updatePos L (x,y) = (x - 1, y)
updatePos R (x,y) = (x + 1, y)

buttonAt :: Coord -> Maybe Char
buttonAt (x,y)
  | x >= (length grid) || y >= ((length . head) grid) = Nothing
  | x < 0 || y < 0 = Nothing
  | otherwise = (grid !! y) !! x

applyRow :: Coord -> [Move] -> Coord
applyRow = foldl move

applyMoves :: Coord -> [[Move]] -> [Coord]
applyMoves = scanl applyRow

main = do
  instrs <- parseCode <$> ((head <$> getArgs) >>= readFile)
  print $ (mapM buttonAt . tail) <$> applyMoves (0,2) <$> instrs
