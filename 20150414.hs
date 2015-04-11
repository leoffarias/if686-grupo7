----------- TRABALHO 07 -------------

-- Questao 1 --

junta :: (t -> t) -> (t -> t) -> (t -> t)
junta g f = f'
 where f' x = g (f x)

compose :: (t -> t) -> [(t -> t)] -> [(t -> t)]
compose g f = map (junta g) f