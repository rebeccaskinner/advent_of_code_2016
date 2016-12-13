{-# LANGUAGE TupleSections #-}
import Data.Maybe
import Data.Char
import System.Environment
import Data.List.Split
import Data.List
import Text.Printf
import Text.Read

data CompressTree = CompressLeaf String | CompressNode Int [CompressTree] deriving (Eq, Show)

type CompressedDocument = [CompressTree]

leaf = CompressLeaf
node = (`CompressNode` [])

emptyTree = CompressLeaf ""

mkNode' :: String -> [CompressTree]
mkNode' s = map nodeFromSplit (splitNodes s)

mkNode = concatMap explodeNode . mkNode'

nodeFromSplit :: String -> CompressTree
nodeFromSplit s =
  case parseHdr s of
    Just (hdrLen, takeLen, mult) ->
      CompressNode mult [(CompressLeaf (drop hdrLen s))]
    Nothing -> CompressLeaf s

explodeNode :: CompressTree -> [CompressTree]
explodeNode (CompressNode x s) = [CompressNode x (concatMap explodeNode s)]
explodeNode (CompressLeaf "") = [CompressLeaf ""]
explodeNode (CompressLeaf s) =
  let nodes = splitNodes s in
    if (length nodes == 1) && (isNothing $ parseHdr (nodes !! 0)) then
      [CompressLeaf s]
    else
      mkNode s


splitNodes :: String -> [String]
splitNodes = reverse . filter (not . null) . splitNodes' []
  where
    splitNodes' :: [String] -> String -> [String]
    splitNodes' accum "" = accum
    splitNodes' accum s =
      let (nextAccum, nextParse) = break (=='(') s in
        case parseHdr nextParse of
          Just (hdrLen, takeLen, mult) ->
            let (thisAccum, s') = splitAt (hdrLen + takeLen) nextParse in
              splitNodes' (thisAccum : nextAccum : accum) s'
          Nothing -> splitNodes' (nextAccum : accum) nextParse


getHdr :: String -> String
getHdr = (takeWhile (/= ')') . dropWhile (=='('))

decompressNode (CompressLeaf s) = s
decompressNode (CompressNode x n) =
  concat . replicate x $ concatMap decompressNode n

decompress :: CompressedDocument -> String
decompress = concatMap decompressNode

showCompressed :: CompressTree -> String
showCompressed (CompressLeaf s) = s
showCompressed (CompressNode mult nodes) =
  let inner = concatMap showCompressed nodes in
    printf "(%dx%d)%s" (length inner) mult inner

compress :: CompressedDocument -> String
compress = concatMap showCompressed

docSize :: CompressedDocument -> Int
docSize = sum . map sumExpansion

parseHdr :: String -> Maybe (Int, Int, Int)
parseHdr s =
  let s' = getHdr s
      l = length s' + 2
  in
    case wordsBy (=='x') s' of
      [a,b] ->
        let a' = readMaybe a
            b' = readMaybe b
        in (\(a,b) -> (l,a,b)) <$> ((,) <$> a' <*> b')
      _ -> Nothing

breakHdr :: String -> (String, String)
breakHdr = breakHdr' ""
  where
    breakHdr' :: String -> String -> (String, String)
    breakHdr' accum "" = (reverse accum, "")
    breakHdr' accum s@(x:xs) =
      case parseHdr s of
        Nothing -> breakHdr' (x:accum) xs
        Just _ -> (accum, s)

sumExpansion :: CompressTree -> Int
sumExpansion (CompressLeaf s) = length s
sumExpansion (CompressNode l x)  = l * (sum . map sumExpansion $ x)

expandedLen :: String -> Int
expandedLen = docSize . mkNode

docLength :: CompressedDocument -> Int
docLength = sum . map sumExpansion

main = (expandedLen . filter (not . isSpace)) <$> ((head <$> getArgs) >>= readFile) >>= print
