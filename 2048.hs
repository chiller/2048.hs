import System.IO
import Util2048


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

