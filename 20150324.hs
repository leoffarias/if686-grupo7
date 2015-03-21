length' :: [Int] -> Int
length' [] = 0
length' (a:as) = (length' as) + 1

esquerda :: [Int] -> Int -> [Int]
esquerda (a:as) 1 = [a]
esquerda (a:as) n = a:(esquerda as (n-1))

direita :: [Int] -> Int -> [Int]
direita (a:as) 1 = as
direita (a:as) n = direita as (n-1)

merge :: [Int] -> [Int] -> [Int]
merge [] a = a
merge a [] = a
merge (a:as) (b:bs)
 | a <= b = a : merge as (b:bs)
 | otherwise = b : merge (a:as) bs

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort (a:[]) = [a]
mergeSort as = merge (mergeSort (esquerda as ((length' as) `div` 2))) (mergeSort (direita as ((length' as) `div` 2)))