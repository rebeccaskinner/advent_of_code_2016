import Data.Char
import System.IO
import System.Environment
import Data.Maybe
import Text.Read
import Data.List.Split
import Data.List

data Room = Room [String] Int String deriving (Eq, Show)

parseRoom :: String -> Maybe Room
parseRoom s =
  let (end : roomStrs) = reverse $ wordsBy (== '-') s
      results = parseRoomEnd end
  in (uncurry $ Room (reverse roomStrs)) <$> results

parseRoomEnd :: String -> Maybe (Int, String)
parseRoomEnd s =
  let (roomID : end') = wordsBy (=='[') s
      end'' = end' !! 0
      roomEnd = (reverse . tail . reverse) end''
      roomID' = readMaybe roomID :: Maybe Int
  in (flip (,) roomEnd) <$> roomID'

reifyRoom :: Room -> Maybe Room
reifyRoom r@(Room rooms num chksum) =
    if getChecksum rooms == chksum then Just r else Nothing

getChecksum :: [String] -> String
getChecksum =
  take 5
  . map head
  . concat
  . map sort
  . groupBy (\a b -> (length a) == (length b))
  . reverse
  . sortOn length
  . group
  . sort
  . concat

run :: String -> Int
run s =
  let rooms = catMaybes $ map ((>>= reifyRoom) . parseRoom) $ lines s
      roomNumbers = map getRoomNumber rooms
  in sum roomNumbers
  where
    getRoomNumber (Room _ num _) = num

shiftLetter :: Char -> Int -> Char
shiftLetter '-' _ = ' '
shiftLetter letter count =
  let alphabet = cycle $ ['a'..'z']
      letter' = (ord . toLower $ letter) `mod` (ord 'a')
  in alphabet !! (count + letter')

decryptRoom :: Room -> Room
decryptRoom (Room encrypted count chksm) =
  let rooms' = map (map (\x -> shiftLetter x count)) encrypted in
    Room rooms' count chksm

run' :: String -> [String]
run' s =
  let rooms = catMaybes $ map ((>>= reifyRoom) . parseRoom) $ lines s in
    map (showRoom .decryptRoom) rooms

showRoom (Room fields sectorID _) = unwords fields ++ "\t" ++ (show sectorID)

main = do
  input <- (head <$> getArgs) >>= readFile
  print $ run input
  mapM putStrLn $ run' input
