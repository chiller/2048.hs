module Util2048 where
import Data.List
import System.Random
import Data.List.Split
import Data.Char

printTbl :: [[Int]] -> String
printTbl  = unlines . map (unwords . map show)

merge :: [Int] -> [Int]
merge (x:y:xs) 
    | x == y = [x+x, 0] ++ merge(xs)
    | otherwise = [x] ++ merge(y:xs)
merge x = x

shift :: [Int] -> [Int]
shift xs = filter (\x -> x /= 0) xs ++ filter (\x -> x == 0) xs 

mergebase :: [Int] -> [Int]
mergebase = shift . merge . shift

mergeleft :: [[Int]] -> [[Int]]
mergeleft = map mergebase

mergeright :: [[Int]] -> [[Int]]
mergeright = map $ reverse . mergebase . reverse

mergeup :: [[Int]] -> [[Int]]
mergeup = transpose . mergeleft . transpose

mergedown :: [[Int]] -> [[Int]]
mergedown = transpose . mergeright . transpose

unflatten :: Int -> [Int] -> [[Int]]
unflatten _ [] = []
unflatten i xs = [take i xs] ++ unflatten i (drop i xs)

update1 :: Int -> [Int] -> [Int]
update1 0 (0:xs) = (1:xs)
update1 i (0:xs) = 0:(update1 (i-1) xs)
update1 i (x:xs) = x:(update1 i xs)
update1 _ [] = []

zeroes = length . filter (\x -> x == 0)

--todo: refactor me
initTbl :: Int -> IO [[Int]]
initTbl size = do
    g <- getStdGen
    return $ unflatten size $ map (digitToInt . head) $tail $ splitOn ""  $ take (size^2) (randomRs ('0', '1') g)

rollDice :: Int -> IO Int
rollDice i = getStdRandom (randomR (0,i))


flipone row = do
    i <- rollDice $ (zeroes row) - 1
    return $ update1 i row

fliptbl tbl = do
  row <-  flipone $ concat tbl
  return $ unflatten (length tbl) row








