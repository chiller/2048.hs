module Lib where

import Data.List (transpose, maximumBy)
import System.Random
import Data.List.Split
import Data.Char
import Data.Ord
import Control.Monad.Trans.Writer.Lazy (tell, Writer, runWriter)
import Control.Monad (liftM, (<=<))


printTbl :: [[Int]] -> String
printTbl xs =  unlines . map unwords $ unflatten (length xs) $ pad . map show . concat $ xs

mergeWithScore :: [Int] -> ([Int], Int)
mergeWithScore (x:y:xs)
    | x == y = let (mxs, sc) = mergeWithScore xs in ([x+x, 0] ++ mxs, x +x + sc)
    | otherwise = let (myxs, sc) = mergeWithScore (y:xs) in (x:myxs, sc)
mergeWithScore x = (x, 0)

instance Monoid Int where
    mempty  = 0
    mappend = (+)

mergeM :: [Int] -> Writer Int [Int]
mergeM xs = do
    let (xs', score) = mergeWithScore xs
    tell score
    return xs'

shift :: [Int] -> [Int]
shift xs = filter (/= 0) xs ++ filter (== 0) xs

shiftM :: [Int] -> Writer Int [Int]
shiftM xs = return (shift xs)

mergebaseM :: [Int] -> Writer Int [Int]
mergebaseM = shiftM <=< mergeM <=< shiftM

mergeleftM :: [[Int]] -> Writer Int [[Int]]
mergeleftM = mapM mergebaseM

mergerightM :: [[Int]] -> Writer Int [[Int]]
mergerightM = mapM $ (return . reverse) <=< mergebaseM <=< (return . reverse)

mergeupM :: [[Int]] -> Writer Int [[Int]]
mergeupM = (return . transpose) <=< mergeleftM <=< ( return . transpose )

mergedownM :: [[Int]] -> Writer Int [[Int]]
mergedownM = (return . transpose) <=< mergerightM <=< ( return . transpose )


unflatten :: Int -> [a] -> [[a]]
unflatten _ [] = []
unflatten i xs = [take i xs] ++ unflatten i (drop i xs)

update1 :: Int -> [Int] -> [Int]
update1 0 (0:xs) = (1:xs)
update1 i (0:xs) = 0:(update1 (i-1) xs)
update1 i (x:xs) = x:(update1 i xs)
update1 _ [] = []

zeroes = length . filter (\x -> x == 0)

padto i str = replicate (i - length str) ' ' ++ str

pad :: [String] -> [String]
pad xs = map (padto size) xs
    where size = length $ maximumBy (comparing length) xs

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







