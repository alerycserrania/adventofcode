import Data.List
import Data.Maybe
import Data.Ord
import Data.Either
import System.IO

main = do
  input <- getInput
  let dividers = ["[[2]]", "[[6]]"]
  let packets = map (head . parseList) . concat . splitOn [] $ (input ++ dividers)
  let sorted = sortBy compRecList packets
  print (product . map fst . filter ((\p -> elem p dividers) . snd) . zip [1..] . map showRecursiveList $ sorted) 

data RecList a = RecList (Either a [RecList a])
eitherR a b (RecList c) = either a b c

compRecList e1 e2 = eitherR (\num -> compNum num e2) (\list -> compList list e2) e1

compNum numA e2 = eitherR (\numB -> compNumNum numA numB ) (\listB -> compListList ([RecList (Left numA)]) listB) e2

compList listA e2 = eitherR (\numB -> compListList listA [RecList (Left numB)]) (\listB -> compListList listA listB) e2

compNumNum numA numB = compare numA numB

compListList listA listB
  | null listA && null listB = EQ
  | null listA = LT
  | null listB = GT
  | c /= EQ = c
  | otherwise = compListList (tail listA) (tail listB)
    where c = compRecList (head listA) (head listB) 

showRecursiveList (RecList e) = either (\s -> show s) (\l -> "[" ++ (intercalate ", " . map showRecursiveList $ l) ++ "]") e

parseList li = map (mp) (splitList li)
  where
    mp el
      | null el = RecList (Right [])
      | (head el == '[') && (last el == ']') = RecList (Right (parseList (tail . init $ el)))
      | otherwise = RecList (Left (read el :: Int))

splitList li
  | null li = []
  | otherwise = el : splitList (rest)
  where el =  takeElement li
        rest = drop (length el + 1) li

takeElement li = takeElement' li 0
  where 
    takeElement' li' i
      | null li' = []
      | head li' == ',' && i == 0 = []
      | head li' == ']' && i == 0 = []
      | head li' == '[' = head li' : takeElement' (tail li') (i+1)
      | head li' == ']' = head li' : takeElement' (tail li') (i-1)
      | otherwise = head li' : takeElement' (tail li') (i)

splitOn e ls
  | isJust mpos = take pos ls : splitOn e (drop (pos + 1) ls)
  | otherwise = [ls]
  where mpos = elemIndex e ls
        pos = fromJust mpos

getInput = do
  done <- isEOF
  if done
    then return []
    else do
      line <- getLine
      rest <- getInput
      return (line : rest)