----------- questao 2 -------------

{-showLook :: String -> Int
showLook a:as
 | a == (head as) = 1 + showLook as

look :: Int -> String -- n, int ou t?
look 1 = "1"
look n = [head (show n)]-}
{-
look :: Int -> String
look 1 = quebraLook "1" 1
look n = quebraLook n 1-}

quebraLook :: String -> Int -> String
quebraLook [] _ = []
quebraLook (a:[]) n = (show n) ++ [a]
quebraLook (a:as) n
 | a == (head as) = quebraLook as (1+n)
 | otherwise = (show n) ++ [a] ++ quebraLook as 1