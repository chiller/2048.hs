module Main where

import Lib

import System.Console.ANSI
import System.IO

main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  tbl <- initTbl 4
  loop tbl
      where loop tbl = do
              tp <- fliptbl tbl
              clearScreen
              putStrLn $  printTbl tp
              c <- hGetChar stdin
              case c of
                'q' -> return ()
                'a' -> loop $ mergeleft tp
                'd' -> loop $ mergeright tp
                'w' -> loop $ mergeup tp
                's' -> loop $ mergedown tp
                input -> loop tp

