{-# LANGUAGE TupleSections #-}
import Data.Char
import Text.Read
import System.Environment
import System.IO
import Debug.Trace
import Control.Parallel.Strategies
import Data.List.Split
import Data.Maybe

import Data.Hash.MD5

run input = take 8 . catMaybes $ chunkMap 4 (getPasswordChar input) [0,1..]

chunkMap :: NFData b => Int -> (a -> b) -> [a] -> [b]
chunkMap chunkSize f lst =
  concat . map (parMap rdeepseq f) $ chunksOf chunkSize lst

getPasswordChar :: String -> Int -> Maybe Char
getPasswordChar pfx idx =
  let checksum = md5s (Str $ pfx ++ (show idx)) in
    if leadingZeros checksum
    then Just (checksum !! 5)
    else Nothing

getPasswordChar' :: String -> Int -> Maybe (Int, Char)
getPasswordChar' str idx =
    let checksum = md5s (Str $ str ++ (show idx))
        pwIndex = checksum !! 5
        pwChar = checksum !! 6
        isValidIndex = pwIndex `elem` ['0'..'7']
    in
      if leadingZeros checksum && isValidIndex then
        Just ((ord pwIndex) - (ord '0'), pwChar)
      else Nothing

run'' :: [Maybe Char] -> Int -> Int -> String -> String
run'' accum idx expectedLen input
  | (length . catMaybes) accum == expectedLen = catMaybes accum
  | otherwise =
    let accum' =
          case getPasswordChar' input idx of
            Just (pwIndex, pwChr) ->
              trace ("Got a password char: " ++ [pwChr] ++ " at " ++ (show pwIndex)) $
              updateIfEmpty accum pwIndex pwChr
            Nothing -> accum
    in run'' accum' (idx + 1) expectedLen input
    where
      updateIfEmpty toUpdate fieldNum fieldVal =
        case toUpdate !! fieldNum of
          Nothing ->
            trace ("adding: " ++ [fieldVal] ++ " at " ++ show fieldNum)
            updateAt toUpdate fieldNum (Just fieldVal)
          otherwise -> trace ("skipping: " ++ show fieldNum) $ toUpdate

updateAt :: [a] -> Int -> a -> [a]
updateAt orig 0 new = (new : ) $ tail orig
updateAt orig at new =
  let (front, back) = splitAt (at + 1) orig in
    if length orig > at then
      (reverse . (new : ) . tail . reverse $ front) ++ back
    else orig

run' input = run'' (replicate 8 Nothing) 0 8 input

leadingZeros :: String -> Bool
leadingZeros str = (take 5 str) == (replicate 5 '0')

main = (run' . head <$> getArgs) >>= print
