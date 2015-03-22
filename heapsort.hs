getElement :: Int -> [Int] -> Int -- retorna o numero da lista dada uma posicao, considerando que a primeira posicao = 1
getElement 1 (a:as) = a
getElement position (a:as) = getElement (position - 1) (as)

getPai :: Int -> [Int] -> Int -- retorna o no pai de um numero dada sua posicao
getPai 1 (a:as) = a
getPai position (a:as) = getElement (position `div` 2) (a:as) -- pai = numero de posicao n/2 

length' :: [Int] -> Int -- retorna o tamanho da lista
length' [] = 0
length' (a:as) = (length' as) + 1

createHeap :: Int -> [Int] -> [Int] -- cria o heap dada uma entrada
createHeap position list
    | position == (length' list) + 1 = list
    | getElement position list <=  getPai position list = createHeap (position + 1) list
    | otherwise = createHeap (position `div` 2) (swap (position `div` 2) position list) -- retona a criacao a partir da menor posicao para verificar se precisa de swap novamente

swap :: Int -> Int -> [Int] -> [Int] -- faz a troca entre dois elementos do heap
swap position1 position2 list =  inicio position1 list ++ [getElement position2 list] ++ meio position1 position2 list ++ [getElement position1 list] ++ fim position2 list

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

sort :: [Int] -> [Int] -- elimina o maior elemento do heap e recria o heap ate a lista estar ordenada
sort [] = []
sort list = heapSort (createHeap 1 (inicio (length' list) (swap 1 (length' list) list))) ++ [getElement 1 list]

heapSort :: [Int] -> [Int] -- metodo heapsort que cria o primeiro heap e depois chama o sort
heapSort [] = []
heapSort list = sort (createHeap 1 list)