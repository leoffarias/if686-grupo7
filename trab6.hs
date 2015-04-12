data Grafos t = Nil | Grafo [(t, [(t,Int)])] deriving (Show, Eq) -- grafico representado por vertice e lista de (adjacencente, peso)

-- para o map e o fold, consideramos que as funções são aplicadas nos vertices

{-
-- acho que ta errado, nao sei como testar essa funcao ai
mapGraph :: (Grafos t -> Grafos t) -> Grafos t -> Grafos t
mapGraph f Nil = Nil
mapGraph f (Grafo ((vertice, adjacencias):as)) =  Grafo ( (f vertice, adjacencias) : mapGraph f (Grafo (as)) )
-}

-- acho que ta errado, nao sei como testar essa funcao ai
foldGraph :: (t -> (t,[(t,t)]) -> (t,[(t,t)])) -> t -> Grafos t -> (t,[(t,t)])
foldGraph f t Nil = (t,[])
foldGraph f s (Grafo ((vertice, adjacencias):as)) = f (vertice) (foldGraph f s (Grafo (as)))


{-
foldGraph :: (t -> [t] -> [t]) -> t -> Grafos t -> [t]
foldGraph f t Nil = [t]
foldGraph f s (Grafo ((vertice, adjacencias):as)) = f (vertice) (foldGraph f s (Grafo (as)))
-}

{-
foldGraph :: (t -> [Int] -> [Int]) -> Grafos t -> [Int]
foldGraph f Nil = []
foldGraph f (Grafo ((vertice, adjacencias):as)) = f (vertice) (foldGraph f (Grafo (as))
-}f