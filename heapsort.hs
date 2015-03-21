getElement :: Int -> [Int] -> Int
getElement 1 (a:as) = a
getElement position (a:as) = getElement (position - 1) (as)


getPai :: Int -> [Int] -> Int
getPai 1 (a:as) = a
getPai position (a:as) = getElement (position `div` 2) (a:as)

createTree :: Int -> [Int] -> [Int]
createTree position [] = []
createTree position list
    | getElement position list <=  getPai position list = create position + 1 list
    | otherwise = 0 -- criar funcao SWAP