----------- TRABALHO 06 -------------

----------- questao 1 -------------

----------- questao 2 -------------

{-
type Grafo = [(vertice, [adjacencias])], ou seja, [(t, [t])]
-}

{-
Exemplo de grafo: [(1,[2,3]), (2,[1,3,4]), (3,[1,2,4]), (4,[2,3]), (5,[])]
-}

listaVertices :: (Eq t) => [(t,[t])] -> [(t,Bool)] -- constroi a lista de vertices com a flag booleana "visitado" = False, usando o grafo como entrada
listaVertices [] = []
listaVertices ((x,y):as) = [(x,False)] ++ (listaVertices as)

marcaVertices :: (Eq t) => [(t,Bool)] -> t -> Bool -> [(t,Bool)] -- altera "visitado" usando como entrada a lista de vertices, o vertice e True or False
marcaVertices [] vertice visitado = []
marcaVertices ((x,y):as) vertice visitado
    | x == vertice = [(x,visitado)] ++ (marcaVertices as vertice visitado)
    | otherwise = [(x,y)] ++ (marcaVertices as vertice visitado)

adjacentes :: (Eq t) => [(t,[t])] -> t -> [t] -- retorna a lista de vertices adjacents a um vertice t usando o grafo como entrada
adjacentes [] vertice = []
adjacentes ((x,y):as) vertice
    | x == vertice = y
    | otherwise = adjacentes as vertice

visitado :: (Eq t) => [(t,Bool)] -> t -> Bool -- retorna o estado de "visitado" de um vértice usando a lista de vertices como entrada
visitado [] vertice = False
visitado ((x,y):as) vertice
    | x == vertice = y
    | otherwise = visitado as vertice

proximoAdjacente :: (Eq t) => [t] -> [(t,Bool)] -> [t] -- retorna o proximo vertice adjacente não visitado, entradas: listas de adjacentes e de vertices
proximoAdjacente [] vertices = []
proximoAdjacente (a:as) vertices
    | visitado vertices a == True = proximoAdjacente as vertices
    | otherwise = [a]

busca :: (Eq t) => [(t,[t])] -> [(t,Bool)] -> [t] -> t -> t -> Bool -- funcao de busca em profundidade
busca grafo vertices [] inicio fim = False -- caso base de a pilha estar vazia (terem acabado os vertices adjacentes não visitados)
busca grafo vertices pilha inicio fim
    | (proximoAdjacente (adjacentes grafo inicio) vertices) == [] = busca grafo vertices (tail pilha) inicio fim -- não há adjacente válido, volta a pilha
    | head (proximoAdjacente (adjacentes grafo inicio) vertices) == fim = True -- chegou no vertice pretendido, retorna o caminho
    | otherwise = busca grafo (marcaVertices vertices inicio True) ((proximoAdjacente (adjacentes grafo inicio) vertices)++pilha) (head (proximoAdjacente (adjacentes grafo inicio) vertices)) fim 
    -- marca o proximo vertice adjacente como visitado, o coloca na pilha e o considera como inicio

search :: (Eq t) => [(t,[t])] -> t -> t -> Bool -- funcao inicial que marca vertices como não lidos, define o incial como visitado e o coloca na fila
search grafo inicio fim = busca grafo (marcaVertices (listaVertices grafo) inicio True) [inicio] inicio fim
