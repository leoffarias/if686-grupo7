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
type Grafo = [(vertice, [adjacencias])], ou seja, [(t, [t])]
-}

{-
Exemplo de grafo: [(1,[2,3]), (2,[1,3,4]), (3,[1,2,4]), (4,[2,3]), (5,[])], querendo percorrer entre nó 1 e 4
-}

listaVertices :: (Eq t) => [(t,[t])] -> [(t,Bool)] -- constroi a lista de vertices com a flag booleana de visitado = False
listaVertices [] = [] -- caso base
listaVertices ((x,y):as) = [(x,False)] ++ (listaVertices as)

marcaVertices :: (Eq t) => [(t,Bool)] -> t -> Bool -> [(t,Bool)]
marcaVertices [] vertice visitado = [] -- caso base
marcaVertices ((x,y):as) vertice visitado
    | x == vertice = [(x,visitado)] ++ (marcaVertices as vertice visitado)
    | otherwise = [(x,y)] ++ (marcaVertices as vertice visitado)

busca :: (Eq t) => [(t,[t])] -> [(t,Bool)] -> [t] -> t -> t -> [(t,t)] -- funcao de busca em profundidade
busca grafo vertices []] inicio fim = [] -- caso base de a pilha estar vazia (terem acabado os vertices adjacentes nao visitados)
busca grafo vertices pilha inicio fim
    -- coloca o primeiro vertice adjacente na pilha, marca como visitado e verifica se é igual ao "fim" assim por diante
    -- se o vertice adjacente for igual ao fim, cria o caminho com os pontos da pilha e devolve para a funcao search
    -- se nao tiver vertice adjancete [nao visitado], tira o numero da pilha e procura um adjacente ao anterior, assim por diante

search :: (Eq t) => [(t,[t])] -> t -> t -> [(t,t)] -- funcao inicial que marca vertices como não lidos, define o incial como visitado e o coloca na fila
search grafo inicio fim = busca grafo (marcaVertices (listaVertices grafo) inicio True) [inicio] inicio fim

-- https://www.youtube.com/watch?v=pJ3ilnhXWCQ
-- http://www.land.ufrj.br/~classes/grafos/slides/aula_6.pdf

