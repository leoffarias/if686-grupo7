getElement :: Int -> [Int] -> Int -- retorna o numero da lista dada uma posicao
getElement 1 (a:as) = a -- caso base, considera o primeira posicao como de numero 1
getElement position (a:as) = getElement (position - 1) (as)


getPai :: Int -> [Int] -> Int -- retorna o no pai de um numero dada sua posicao
getPai 1 (a:as) = a -- caso base, o primeiro numero sendo pai dele mesmo
getPai position (a:as) = getElement (position `div` 2) (a:as) -- pai = numero de posicao n/2 

length' :: [Int] -> Int -- retorna o tamanho da lista
length' [] = 0
length' (a:as) = (length' as) + 1

createTree :: Int -> [Int] -> [Int] -- cria uma arvore inicial dada uma entrada
createTree position list
  | position == (length' list) + 1 = list -- caso base de chegar no fim da lista ou ela ser vazia
  | getElement position list <=  getPai position list = create (position + 1) list
  | otherwise = 0 -- criar funcao SWAP

swap :: Int -> [Int] -> [Int]
