-- Trabalho 8

qsort :: Ord a => Num a => [a] -> [a]
qsort [] = []
qsort (a:as) = qsort menor ++ [a] ++ qsort maior
    where menor  = [ x | x <- as, x < a ]
          maior = [ x | x <- as, x >= a ]

filtro :: Ord a => Num a => (a -> Bool) -> (a -> Bool) -> [a] -> [a] -> [[a]]
filtro p f (a:[]) bs = qsort (filtro2 p f bs) : [qsort(filter (>a) bs)]
filtro p f (a:as) bs = qsort (filtro2 p f bs) : filtro (>a) (<= (head as)) as bs

filtro2 p f l = [a | a <- l, (p a) && (f a)]

listPartitioner :: Ord a => Num a => [a] -> ([a] -> [[a]])
listPartitioner (a:as) = filtro (<=a) (<=a) (qsort (a:as))

{-filtro :: a -> (a -> Bool) -> [a] -> [[a]]
filtro n p [] = [[]]
filtro n p (a:as) 
 | n p a = [a] : filtro p as
 | otherwise = filtro p as-}

{-filtro :: (a -> Bool) -> a -> [a]
filtro p n
 | p n = [n]
 | otherwise = []

listPartitioner :: Num a => [a] -> ([a] -> [[a]])
--listPartitioner [] = ([] . [[]])
listPartitioner (a:as) = (filtro (< a)) ++ listPartitioner as

-}

--filtro :: (a -> Bool) -> a -> [a]
{-}
filtro :: Ord a => [a] -> [a] -> [[a]]
filtro [] bs = [bs]
filtro as [] = [as]
filtro (a:as) (b:bs)
 | (b < a) = [b : head (filtro (a:as) bs)]
 | otherwise = filtro as (b:bs)
-}
--listPartitioner :: Num a => [a] -> [a] -> [[a]]
--listPartitioner (a:as) (b:bs) =
{-}
filtro :: Ord a => [a] -> [a] -> [a]
filtro [] bs = bs
filtro (a:as) (b:bs) = (filter (<a) (b:bs))-}

{-filtro :: Ord a => [a] -> [a] -> [[a]]
filtro [] bs = [bs]
filtro (a:as) (b:bs) = (filter (<=a) (b:bs)) : filtro as (b:bs)-}