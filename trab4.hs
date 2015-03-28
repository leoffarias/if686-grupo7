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