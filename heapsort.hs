getElement :: Int -> [Int] -> Int -- considera a raiz como posicao 1
getElement 1 (a:as) = a
getElement position (a:as) = getElement (position - 1) (as)


getPai :: Int -> [Int] -> Int
getPai 1 (a:as) = a
getPai position (a:as) = getElement (position `div` 2) (a:as)

length' :: [Int] -> Int
length' [] = 0
length' (a:as) = (length' as) + 1

createTree :: Int -> [Int] -> [Int]
createTree position [] = []
createTree position list
    | position = length + 1 = list
    | getElement position list <=  getPai position list = create (position + 1) list
    | otherwise = 0 -- criar funcao SWAP

swap :: Int -> [Int] -> [Int]
