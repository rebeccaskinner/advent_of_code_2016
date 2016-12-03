import System.Environment (getArgs)
import System.IO (readFile)
import Data.List (permutations, transpose)
import Text.Read (readMaybe)
import Data.List.Split

isTriangle :: [Int] -> Bool
isTriangle =
  all id . map isTriangle' . permutations
  where
    isTriangle' (x:xs) = x < sum xs

parseRow :: String -> Maybe [Int]
parseRow = mapM readMaybe . words

parseFile :: String -> Maybe [[Int]]
parseFile = mapM parseRow . lines

parseFile' :: String -> Maybe [[Int]]
parseFile' =  mapM (mapM readMaybe) . chunksOf 3 . concat . transpose . map words . lines

run :: (String -> Maybe [[Int]]) -> String -> Maybe String
run f  input =
  let parsed = f input in
    (show . length . filter id . map isTriangle) <$> parsed

run1 :: String -> Maybe String
run1 = run parseFile

run2 :: String -> Maybe String
run2 = run parseFile'

main = do
  input <- (head <$> getArgs) >>= readFile
  print $ run1 input
  print $ run2 input
