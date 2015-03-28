----------- questao 1 -------------

{- 
O polimorfismo de sobrecarga ocorre quando temos funções de mesmo nome, porém distintas para cada tipo. Em Java, isso é feito através de criação de 
métodos de mesmo nome, porém de assinaturas diferentes, seja pelo número ou tipo de argumentos. Já em Haskell, isso é feito através da utilização 
de classes de tipos, que permitem definir diferentes tipos para uma função ou operação. Dessa forma, se por um lado em Haskell fica mais simples 
definirmos uma função que utiliza classes de tipos para suportar a sobrecarga, ao invés de mais de um método de diferentes argumentos; por outro 
lado, acaba sendo mais custoso por ser necessário realizar o processo de unificação, que é a correspondência entre os argumentos e parâmetros passados.

Referências

CASTOR, Fernando. Polimorfismo. Disponível em: <https://docs.google.com/a/cin.ufpe.br/viewer?a=v&pid=sites&srcid=Y2luLnVmcGUuYnJ8aWY2ODZ8Z3g6NDYwNmI3ZGMxNDExMjFlOQ>. Acesso em: 28 mar. 2015.

MEDEIROS, Higor. Uso de Polimorfismo em Java. Disponível em: <http://www.devmedia.com.br/uso-de-polimorfismo-em-java/26140>. Acesso em: 28 mar. 2015.

OLIVEIRA, Guilherme Gomes Neves de; PETRI, Renzo Augusto Lapelligrini. Haskell: Seminário de Linguagens de Programação. Disponível em: <http://pt.slideshare.net/renzopetri/seminario-haskell>. Acesso em: 28 mar. 2015.
-}

----------- questao 2 -------------

quebraLook :: String -> Int -> String
quebraLook (a:[]) n = (show n) ++ [a]
quebraLook (a:as) n
 | a == (head as) = quebraLook as (1+n)
 | otherwise = (show n) ++ [a] ++ quebraLook as 1

repeat' :: String -> Int -> String
repeat' s 1 = "1"
repeat' s 2 = quebraLook s 1
repeat' s n = repeat' (quebraLook s 1) (n-1)

lookAndSay :: Int -> String
lookAndSay n = repeat' "1" n

----------- questao 3 -------------

{-
Defina um tipo de dados que representa um grafo não necessariamente conexo, onde cada nó tem um rótulo:
type Rotulo = qualquer tipo de dado, ou seja, t -- nó
type Aresta = (Rotulo, Rotulo), ou seja, (t,t) -- um aresta é a ligação entre dois nós, caso o nó não possua aresta o segundo rotulo é ele mesmo
type Grafo = [Aresta], ou seja, [(t,t)] -- um grafo pode ser definido por um conjunto de arestas
-}

{-
Exemplo de grafo: [(1,2), (2,3), (1,3), (2,4), (3,4), (5,5)], querendo percorrer entre nó 1 e 4
-}

{-
corrigeGrafo:
É necessário completar o grafo para que se vejam todos os caminhos possíveis, se tem (1,2), por exemplo, tem que ter (2,1).
Além diso, é interessante remover os nós do tipo (x,x), para evitar loops inifinitos
-}
corrigeGrafo :: (Eq t) => [(t,t)] -> [(t,t)]
corrigeGrafo [] = [] -- caso base
corrigeGrafo ((x,y):as)
    | x == y = corrigeGrafo as -- remove os elementos do tipo (x,x)
    | contemAresta ((x,y):as) (y,x) == False = [(x,y),(y,x)] ++ (corrigeGrafo as)
    | otherwise = (x,y):corrigeGrafo as

contemAresta :: (Eq t) => [(t,t)] -> (t,t) -> Bool -- confere se há uma aresta no grafo
contemAresta [] (x,y) = False -- caso base
contemAresta ((x',y'):as) (x,y)
    | (x,y) == (x',y') = True
    | otherwise = contemAresta as (x,y)

marcaVisitado :: (Eq t) => [(t,Bool)] -> t -> [(t,Bool)] 
marcaVisitado [] r = []
marcaVisitado ((r, v):as) rotulo
    | r == rotulo = (r,True):marcaVisitado as rotulo
    | otherwise = (r,v):marcaVisitado as rotulo

buscaAdjacencia :: (Eq t) => [(t, t)] -> t -> [(t)] -- retorna vertices adjacentes a outro vertice (podem haver multiplos caminhos)
buscaAdjacencia grafo r = [ x | (rotulo, x) <- grafo, rotulo == r]

-- falta corrigir/terminar --

busca :: (Eq t) => [(t,t)] -> [(t,Bool)] -> [t] -> t -> t -> [(t,t)] -- funcao de busca em profundidade
busca grafo vertices pilha inicio fim = [] -- terminar!

search :: (Eq t) => [(t,t)] -> t -> t -> [(t,t)] -- funcao inicial que corrige o grafo, lista os vertices, monta a pilha inicial e manda buscar
search grafo inicio fim = busca (corrigeGrafo grafo) (listaVertices (corrigeGrafo grafo)) [inicio] inicio fim

listaVertices :: (Eq t) => [(t,t)] -> [(t,Bool)] -- retorna a lista de vertices com a flag booleana de visitado = False
listaVertices [] = [] -- caso base
listaVertices ((x,y):as) = [(x,False)] ++ (listaVertices as) -- remover vertices repetidos

-- https://www.youtube.com/watch?v=pJ3ilnhXWCQ
-- http://www.land.ufrj.br/~classes/grafos/slides/aula_6.pdf