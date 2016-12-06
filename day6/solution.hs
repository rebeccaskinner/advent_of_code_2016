import Text.Printf
import Data.Char
import Data.List.Split
import Data.List
import System.IO
import System.Environment


run = map (head . last . sortOn length . group . sort) . transpose . lines
run' = map (head . head . sortOn length . group . sort) . transpose . lines
main = do
  input <- head <$> getArgs >>= readFile
  printf "Part 1: %s\n" $ run input
  printf "Part 2: %s\n" $ run' input
