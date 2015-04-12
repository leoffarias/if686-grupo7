----------- TRABALHO 07 -------------

-- Questao 1 --

junta :: (t -> t) -> (t -> t) -> (t -> t)
junta g f = f'
 where f' x = g (f x)

compose :: (t -> t) -> [(t -> t)] -> [(t -> t)]
compose g f = map (junta g) f


-- Questao 2 --

data Grafos t = Nil | Grafo [(t, [(t,Int)])] deriving (Show, Eq) -- grafico representado por vertice e lista de (adjacencente, peso)

{-mapGraph :: (t -> u) -> Grafo -> [u]
mapGraph f Nil = []
mapGraph f (Grafo ((v, ((a,p):bs)):as)) = f a : mapGraph f as-}


-- Questao 3 --

data Tree t = NilT | Node t (Tree t) (Tree t) deriving (Eq, Show)

--(Node 5 (Node 7 (Node 15 NilT (Node 6 NilT NilT)) (Node 2 NilT NilT)) (Node 10 NilT NilT))

--[(Node 5 (Node 7 NilT (Node 2 NilT NilT))), (Node 6 NilT NilT)]

--filterTree :: Tree -> (t -> Bool) -> [Tree]

{-filter' :: (t -> Bool) -> [t] -> [t]  
filter' _ [] = []  
filter' p (x:xs)   
    | p x       = x : filter' p xs  
    | otherwise = filter' p xs  -}

{-Funcionando pra retornar 1 arvore
filterTree :: (t -> Bool) -> Tree t -> Tree t
filterTree _ NilT = NilT
filterTree f (Node v e d)
    | f v = (Node v (filterTree f e) (filterTree f d))
    | otherwise = NilT
-}

{-filtrando :: (t -> Bool) -> Tree t -> Tree t
filtrando _ NilT = NilT
filtrando f (Node v e d)
    | f v = (Node v (filtrando f e) (filtrando f d))
    | not (f v) = NilT

filtro :: (t -> Bool) -> Tree t -> [Tree t]
filtro _ NilT = []
filtro f (Node v e d)
    | f v = (Node v (filtrando f e) (filtrando f d)):[]
    | otherwise = [] -}
{-
filterTree :: (t -> Bool) -> Tree t -> [Tree t]
filterTree _ NilT = []
filterTree f (Node v e d)
 | f v = (Node v (filterTree f e) (filterTree f d)):[]
 | otherwise = []-}
{-
filtro :: Eq t => (t -> Bool) -> Tree t -> [Tree t]
filtro _ NilT = [NilT]
filtro f (Node v e d)
    | f v = (Node v (head (filtro f e)) (head (filtro f d))):[]
    | not (f v) && (e == NilT) && (d == NilT) = [NilT]
    | otherwise = filtro f NilT-}

    {-
filtrando :: (t -> Bool) -> (Tree t, [Tree t]) -> (Tree t, [Tree t])
filtrando _ (x, []) = (x, [])
filtrando f (x, (a:as)) = (x, ([esquerdaFiltrada] ++ [teste]))
    where (esquerdaFiltrada, listaEsq) = filterTree f a
          (direitaFiltrada, listaDir) = filterTree f as
          (teste, listaDirewde) = filtrando f (x, direitaFiltrada)-}

filtro :: Eq t => (t -> Bool) -> Tree t -> (Tree t, [Tree t])
filtro _ NilT = (NilT, [])
filtro f (Node v e d)
    | f v = ((Node v esquerda direita), (listaEsq ++ listaDir))
    | not (f v) && (e == NilT) && (d == NilT) = (NilT, [])
    | otherwise = (NilT, [e] ++ [d] )
    where (esquerda, listaEsq) = filtro f e
          (direita, listaDir) = filtro f d

filtrandoLista :: Eq t => (t -> Bool) -> (Tree t, [Tree t]) -> [Tree t]
filtrandoLista f (x, []) = []
filtrandoLista f (x, a:as) 
 | arv == NilT = filtrandoLista f (x, as)
 | otherwise = arv : filtrandoLista f (x, as)
 where (arv, filtraHead) = filtro f a

filterTree :: Eq t => (t -> Bool) -> Tree t -> [Tree t]
filterTree _ NilT = []
filterTree f a = arv : (filtrandoLista f (arv, floresta))
 where (arv, floresta) = filtro f a
