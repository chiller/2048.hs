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

unflatten _ [] = []
unflatten i xs = [take i xs] ++ unflatten i (drop i xs)


initTbl size = do
    g <- getStdGen
    return $ unflatten size $ map (digitToInt . head) $tail $ splitOn ""  $ take (size^2) (randomRs ('0', '1') g)


