import Data.Char 
merge (x:y:xs) 
    | x == y = [x+x, 0] ++ merge(xs)
    | otherwise = [x] ++ merge(y:xs)
merge x = x

shift xs = filter (\x -> x /= 0) xs ++ filter (\x -> x == 0) xs 

mergeleft = shift . merge . shift
mergeright = reverse . mergeleft . reverse


readinput tbl = do
    print tbl
    x <- getChar
    if x == 'x' then
        return []
    else if x == 'l' then
        readinput (mergeleft tbl)
    else if x == 'r' then
        readinput (mergeright tbl)
    else
        readinput tbl


main = do
    readinput [32,0,32,0] 
    print "done"

