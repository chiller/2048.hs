module Main where

import Lib

import System.Console.ANSI
import System.IO
import Control.Monad.Trans.Writer.Lazy (runWriter)

main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  tbl <- initTbl 4
  loop (tbl, 0)

run func table = runWriter $ func table

loop :: ([[Int]], Int) -> IO ()
loop (tbl, score) = do
      tp <- fliptbl tbl
      clearScreen
      putStrLn $ "Score: " ++ ( show score)
      putStrLn $  printTbl tp
      c <- hGetChar stdin
      let next = case c of
                'q' -> Nothing
                'a' -> Just $ run mergeleftM tp
                'd' -> Just $ run mergerightM tp
                'w' -> Just $ run mergeupM tp
                's' -> Just $ run mergedownM tp
                input -> Just $ (tp, score)
      case next of
        Nothing -> print score >> return ()
        Just (newtable, scoredelta) -> loop (newtable, score + scoredelta)
