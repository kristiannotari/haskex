module RegexHelper where

type Match = (Bool, [Char])

matchChar :: Char -> [Char] -> Match
matchChar _ [] = (False,[])
matchChar c (x:xs) = if c == x then (True,xs) else (False,(x:xs))

thenMatch :: Match -> ([Char] -> Match) -> Match
thenMatch match f =
    case match of 
        (True,rest) -> f rest
        otherwise -> match

firstMatch :: [([Char] -> Match)] -> [Char] -> Match
firstMatch [] input = (False,input)
firstMatch (f:fs) input =
    case f input of
        (True,rest) -> (True,rest)
        otherwise -> firstMatch fs input