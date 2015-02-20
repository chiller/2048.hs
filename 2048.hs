import System.IO
import Util2048

main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  tbl <- initTbl 4
  loop tbl
      where loop tbl = do 
              tp <- fliptbl tbl
              putStrLn $  printTbl tp 
              c <- hGetChar stdin
              case c of
                'q' -> return ()
                'a' -> loop $ mergeleft tp
                'd' -> loop $ mergeright tp
                'w' -> loop $ mergeup tp
                's' -> loop $ mergedown tp
                input -> loop tp

