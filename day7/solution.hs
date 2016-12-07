{-# LANGUAGE DeriveFunctor #-}
import Text.Printf
import System.Environment
import Data.Maybe
import Control.Monad
import Data.List.Split
import Data.List

data SubAddr a = Net a | HyperNet a deriving (Eq, Show, Functor)
type Address = [SubAddr String]

isHypernet :: SubAddr a -> Bool
isHypernet (Net _) = False
isHypernet _ = True

getNet :: SubAddr a -> a
getNet (Net a) = a
getNet (HyperNet a) = a

parseAddress :: String -> Address
parseAddress  = zipWith ($) (cycle [Net, HyperNet]) . concatMap (splitOn "]") . splitOn "["

subAddrOK :: SubAddr String -> Bool
subAddrOK (Net a) = checkString a
subAddrOK (HyperNet a) = not (checkString a)

checkString :: String -> Bool
checkString (a:b:c:d:rest) = ([a,b] == [d,c] && a /= b) || checkString (b:c:d:rest)
checkString _ = False

checkAddress :: Address -> Bool
checkAddress addr =
  let hypers = map subAddrOK $ filter isHypernet addr
      nets = map subAddrOK $ filter (not . isHypernet) addr
  in and hypers && or nets

chunks :: [a] -> [[a]]
chunks = reverse . drop 3 . reverse . map (take 3) . tails

matchingAbaBab :: Address -> Bool
matchingAbaBab addr =
  let (hypers, nets) = partition isHypernet addr
      abas = concatMap (chunks . getNet) nets
      babas = concatMap (chunks . getNet) hypers
  in or [abaMatch x y | x <- abas, y <- babas]
  where
    abaMatch :: String -> String -> Bool
    abaMatch [a,b,c] [x,y,z] = (a == c) && (a == y) && (b == x) && (b == z)
    abaMatch _ _ = False

run input =
  let addrs = filter checkAddress . map parseAddress . lines $ input in
    length addrs

run' input =
  let addrs = filter matchingAbaBab . map parseAddress . lines $ input in
    length addrs

main = do
  input <- (head <$> getArgs) >>= readFile
  printf "Support TLS: %d\n" $ run input
  printf "Support SSL: %d\n" $ run' input
