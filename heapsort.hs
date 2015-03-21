getElement :: Int -> [Int] -> Int -- retorna o numero da lista dada uma posicao
getElement 1 (a:as) = a
getElement position (a:as) = getElement (position - 1) (as)

getPai :: Int -> [Int] -> Int -- retorna o no pai de um numero dada sua posicao
getPai 1 (a:as) = a -- caso base, o primeiro numero sendo pai dele mesmo
getPai position (a:as) = getElement (position `div` 2) (a:as) -- pai = numero de posicao n/2 

length' :: [Int] -> Int -- retorna o tamanho da lista
length' [] = 0
length' (a:as) = (length' as) + 1

createHeap :: Int -> [Int] -> [Int] -- cria o heap dada uma entrada
createHeap position list
    | position == (length' list) + 1 = list
    | getElement position list <=  getPai position list = createHeap (position + 1) list
    | otherwise = swap position list

swap :: Int -> [Int] -> [Int] -- faz a troca caso o no filho tenha numero maior que o pai e retona a criacao a partir do pai para verificar se precisa de swap novamente
swap position list =  createHeap (position `div` 2) (inicio (position `div` 2) list ++ [getElement position list] ++ meio (position `div` 2) position list ++ [getPai position list] ++ fim position list)

inicio :: Int -> [Int] -> [Int] -- auxilia o swap a pegar a lista antes o pai
inicio 1 list = []
inicio position (a:as) = a : inicio (position - 1) as

meio :: Int -> Int -> [Int] -> [Int] -- auxilia o swap a pegar a lista entre pai e filho
meio pai filho (a:as)
    | pai >= 1 = meio (pai - 1) (filho-1) as
    | filho > 1 = a : meio pai (filho-1) as
    | otherwise = []

fim :: Int -> [Int] -> [Int] -- auxilia o swap a pegar a lista depois do filho
fim position (a:as)
    | position > 1 = fim (position - 1) as
    | otherwise = as