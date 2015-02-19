import System.IO
import Util2048


main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  loop [[32,0],[0,0]]
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

