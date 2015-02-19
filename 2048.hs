import System.Console.Haskeline

merge (x:y:xs) 
    | x == y = [x+x, 0] ++ merge(xs)
    | otherwise = [x] ++ merge(y:xs)
merge x = x

shift xs = filter (\x -> x /= 0) xs ++ filter (\x -> x == 0) xs 

mergeleft = shift . merge . shift
mergeright = reverse . mergeleft . reverse

printTbl  = unwords . map show 

main = runInputT defaultSettings (loop [32,0,32,0])
    where 
        loop chr = do
            minput <- getInputChar $ printTbl chr
            case minput of
                Nothing -> return ()
                Just 'q' -> return ()
                Just 'l' -> loop $ mergeleft chr
                Just 'r' -> loop $ mergeright chr
                Just input -> do outputStrLn $ "Input was: " ++ [input]
                                 loop chr
