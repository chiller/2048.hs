import System.IO
import Data.Char

merge (x:y:xs) 
    | x == y = [x+x, 0] ++ merge(xs)
    | otherwise = [x] ++ merge(y:xs)
merge x = x

shift xs = filter (\x -> x /= 0) xs ++ filter (\x -> x == 0) xs 

mergeleft = shift . merge . shift
mergeright = reverse . mergeleft . reverse

printTbl :: [Int] -> String
printTbl  = unwords . map show 


main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  loop [32,0,32,0]
      where loop tbl = do 
              putStrLn $ "\n" ++ printTbl tbl ++ "\n"
              c <- hGetChar stdin
              case c of
                'q' -> return ()
                'a' -> loop $ mergeleft tbl
                'd' -> loop $ mergeright tbl
                input -> loop tbl

