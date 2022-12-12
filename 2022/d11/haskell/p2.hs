import Data.Either
import Data.Maybe
import Data.List
import qualified Data.Map as Map
import Text.Read
import System.IO

main = do
  input <- getInput
  let monkeys = parseMonkeys . split [] $ input
  let inspections = reverse . sort . map (fst) . foldl (\ms _ -> processMonkeys ms) (map (\m -> (0, m)) monkeys) $ (replicate 10000 0)
  print ((uncurry (*)) . (\x -> (x!!0, x!!1)) . take 2 $ inspections)

data Monkey = Monkey {
  items :: [Int],
  mods :: [Map.Map Int Int],
  op :: (Either Int String, String, Either Int String),
  test :: Int,
  whenTrue :: Int,
  whenFalse :: Int
} deriving (Show)

parseMonkeys ms = map updateMonkey monkeys
  where monkeys = map parseMonkey ms
        divs = map (test) monkeys
        updateMonkey monkey = monkey { 
          mods = [Map.fromList . map (\d -> (d, item `mod` d)) $ divs | item <- items monkey] 
        }

parseMonkey m = Monkey {
  items = map (read :: String -> Int) . split ',' . drop 18 $ (m!!1),
  mods = [],
  op = readOperation . split ' ' . drop 19 $ (m!!2),
  test = (read :: String -> Int) . drop 21 $ (m!!3),
  whenTrue = (read :: String -> Int) . drop 29 $ (m!!4),
  whenFalse = (read :: String -> Int) . drop 30 $ (m!!5)
}

processMonkeys monkeys = foldl (processMonkey) monkeys [0..(length monkeys) - 1]


processMonkey monkeys from = [(nb+nbInspected i mk, mk { mods = (applymove i mk) }) | (i, (nb, mk)) <- zip [0..] monkeys]
  where 
    ((to1, it1), (to2, it2)) = throwItems ((map snd monkeys)!!from)
    applymove i monkey 
      | i == from = []
      | i == to1 = mods monkey ++ it1
      | i == to2 = mods monkey ++ it2
      | otherwise = mods monkey
    nbInspected i monkey
      | i == from = length (mods monkey)
      | otherwise = 0


throwItems monkey = ((whenTrue monkey, toTrue), (whenFalse monkey, toFalse))
  where newLevels = map (updateDivs (op monkey)) $ mods monkey
        tests = map (\w -> (w, w Map.! (test monkey) == 0)) newLevels
        (toTrue, toFalse) = foldl (\(ts, fs) (item, b) -> if b then (item : ts, fs) else (ts, item : fs)) ([], []) $ tests

updateDivs op hs = Map.mapWithKey (\k v -> mod (resultOp op v) k) hs

resultOp (l, o, r) old
  | o == "*" = (fromLeft old l) * (fromLeft old r)
  | o == "+" = (fromLeft old l) + (fromLeft old r)

readOperation [a, b, c] = (readOperand a, b , readOperand c)

readOperand i
  | isJust r = Left (fromJust r)
  | otherwise = Right i
    where r = readMaybe i :: Maybe Int

split c l 
  | isJust i = f : split c (tail s)
  | otherwise = [l]
    where i = elemIndex c l
          (f, s) = splitAt (fromJust i) l

getInput = do
  done <- isEOF
  if done
    then return []
    else do
      line <- getLine
      rest <- getInput
      return (line : rest)