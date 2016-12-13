import System.Environment
import Data.List.Split
import Text.Printf
import Text.Read

data ParseState = Passthrough String |
                  ParseExpansion String String |
                  AccumState String Int Int String deriving (Eq, Show)

parseChar :: Char -> ParseState -> Either String ParseState
parseChar c (Passthrough s) =
  Right $ if c == '(' then ParseExpansion s [c] else Passthrough (c:s)
parseChar c (ParseExpansion s' s) =
  if c == ')' then
    case (parseRLE . reverse . init) s of
      Just (distance, amnt) -> Right (AccumState s' amnt distance "")
      Nothing -> Left (printf "parse error: %s is not parsable as a number: " (reverse . init $ s))
  else Right (ParseExpansion s' (c:s))
parseChar c (AccumState s' amnt 1 s) =
  let expanded = concat . replicate amnt $ c : s
  in Right (Passthrough $ expanded ++ s')
parseChar c (AccumState s' amnt left s) =
  Right $ AccumState s' amnt (left - 1) (c:s)

parseStr :: String -> Either String ParseState
parseStr s = foldl (\carry elem -> carry >>= parseChar elem) (return $ Passthrough "") s

parseRLE :: String -> Maybe (Int, Int)
parseRLE s =
  case (sequence . map readMaybe . wordsBy (=='x')) s of
    Just [dist, amnt] -> Just (dist, amnt)
    _ -> Nothing

showParsed :: ParseState -> Either String String
showParsed (Passthrough s) = Right (reverse s)
showParsed p = Left $ "Invalid parse state: " ++ show p

run s = map ((\x -> (x, length x)) <$> (showParsed =<<) . parseStr) $ lines s

main = do
  input <- readFile =<< (head <$> getArgs)
  print $ run input
