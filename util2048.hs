module Util2048 where
import Data.List

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


--main = do 
--    print $ printTbl [[0,1],[0,1]]

