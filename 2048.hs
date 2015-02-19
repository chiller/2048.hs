import System.IO
import Util2048

main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  tbl <- initTbl 4
  loop tbl
      where loop tbl = do 
              putStrLn $  printTbl tbl 
              c <- hGetChar stdin
              case c of
                'q' -> return ()
                'a' -> loop $ mergeleft tbl
                'd' -> loop $ mergeright tbl
                'w' -> loop $ mergeup tbl
                's' -> loop $ mergedown tbl
                input -> loop tbl

