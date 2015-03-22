length' :: [Int] -> Int -- retorna o tamanho da lista
length' [] = 0
length' (a:as) = (length' as) + 1

esquerda :: [Int] -> Int -> [Int] -- retorna os elementos a esquerda de um numero da lista, inclusive o proprio
esquerda (a:as) 1 = [a]
esquerda (a:as) n = a:(esquerda as (n-1))

direita :: [Int] -> Int -> [Int] -- retorna os elementos a direita de um numero da lista, sem incluir o proprio
direita (a:as) 1 = as
direita (a:as) n = direita as (n-1)

merge :: [Int] -> [Int] -> [Int] -- une duas listas, considerando qual o menor elemento dois a dois
merge [] a = a
merge a [] = a
merge (a:as) (b:bs)
 | a <= b = a : merge as (b:bs)
 | otherwise = b : merge (a:as) bs

mergeSort :: [Int] -> [Int] -- quebra a lista em duas e ordena recursivamente
mergeSort [] = []
mergeSort (a:[]) = [a]
mergeSort as = merge (mergeSort (esquerda as ((length' as) `div` 2))) (mergeSort (direita as ((length' as) `div` 2)))