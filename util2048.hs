module Util2048 where

printTbl :: [Int] -> String
printTbl  = unwords . map show 

merge :: [Int] -> [Int]
merge (x:y:xs) 
    | x == y = [x+x, 0] ++ merge(xs)
    | otherwise = [x] ++ merge(y:xs)
merge x = x

shift :: [Int] -> [Int]
shift xs = filter (\x -> x /= 0) xs ++ filter (\x -> x == 0) xs 


mergeleft :: [Int] -> [Int]
mergeleft = shift . merge . shift

mergeright :: [Int] -> [Int]
mergeright = reverse . mergeleft . reverse