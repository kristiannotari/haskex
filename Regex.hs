module Regex where

import Data.Char (isDigit, isLetter)
import RegexHelper (Match, firstMatch, thenMatch, matchChar)

match :: [Char] -> Bool
match input =
    case regex input of
        (True,[]) -> True
        otherwise -> False

regex :: [Char] -> Match
regex [] = (True,[])
regex input =
    case regexHL input of
        (True,[]) -> (True,[])
        (True,rest) -> regexTS rest
        otherwise -> (False,input)

regexHL :: [Char] -> Match
regexHL input = firstMatch [
        (\i -> regexH i `thenMatch` quantifier),
        regexH,
        regexL
    ] input

regexH :: [Char] -> Match
regexH input = firstMatch [
        group,
        set,
        char,
        matchChar '.'
    ] input

regexL :: [Char] -> Match
regexL input = firstMatch [
        matchChar '^',
        matchChar '$'
    ] input

regexTS :: [Char] -> Match
regexTS input =
    case regexT input of
        (True,[]) -> (True,[])
        (True,rest) -> regexTS rest
        otherwise -> (False,input)

regexT :: [Char] -> Match
regexT input = firstMatch [
        regexHL,
        (\i -> (matchChar '|' i) `thenMatch` regexHL)
    ] input

group :: [Char] -> Match
group ('(':xs) =
    case xs of 
        ('?':r1) ->
            case r1 of 
                ('<':r2) ->
                    case name r2 of 
                        (True,('>':r3)) -> go r3
                        otherwise -> (False, ('(':xs))
                (':':r2) -> go r2
                ('=':r2) -> go r2
                ('!':r2) -> go r2
                otherwise -> go r1
        otherwise -> go xs
    where 
        go input = case regex input of
            (True,(')':rest)) -> (True,rest)
            (False,(')':rest)) -> (True,rest)
            otherwise -> (False,input)
group input = (False,input)

set :: [Char] -> Match
set ('[':']':xs) = (True,xs)
set ('[':'^':']':xs) = (True,xs)
set ('[':xs) =
    case xs of 
        ('^':xss) -> go xss
        otherwise -> go xs
    where 
        go input = case setItems input of
            (_,(']':rest)) -> (True,rest)
            otherwise -> (False,input)
set input = (False,input)


setItems :: [Char] -> Match
setItems input =
    case setItem input of
        (True,r1) ->
            let (t,r2) = setItems r1 in
                (True, if t then r2 else r1)
        otherwise -> (False,input)

setItem :: [Char] -> Match
setItem input =
    case setChar input of 
        (True,('-':r1)) ->
            case setChar r1 of
                (True,r2) -> (True,r2)
                (False,_) -> (True,r1)
        (True,rest) -> (True, rest)
        otherwise -> (False, input)

quantifier :: [Char] -> Match
quantifier input =
    case quantifierH input of
        (True,rest) -> lazy rest
        otherwise -> (False,input)
    where
        lazy [] = (True, [])
        lazy (l:rest) = if l == '?' then (True, rest) else (True, (l:rest))

quantifierH :: [Char] -> Match
quantifierH ('?':xs) = (True,xs)
quantifierH ('+':xs) = (True,xs)
quantifierH ('*':xs) = (True,xs)
quantifierH ('{':xs) =
    case number xs of
        (True,('}':rest1)) -> (True, rest1)
        (True,(',':rest1)) ->
            case number rest1 of
                (True,('}':rest2)) -> (True,rest2)
                (False,('}':rest2)) -> (True,rest2)
                otherwise -> (False, ('{':xs))
        otherwise -> (False, ('{':xs))
quantifierH input = (False,input)

name :: [Char] -> Match
name input =
    let
        (t1,r1) = letter input
        (t2,r2) = go r1
    in if t1 then (True, if t2 then r2 else r1) else (False, input)
    where
        go i = 
            let
                (t1,r1) = letter i
                (t2,r2) = digit i
            in case (t1,t2) of
                (True,_) -> (True,r1)
                (False,True) -> (True,r2)
                otherwise -> (False,i)

setChar :: [Char] -> Match
setChar [] = (False,[])
setChar (']':xs) = (False,(']':xs))
setChar (x:xs) = (True,xs)

char :: [Char] -> Match
char [] = (False,[])
char ('\\':x:xs) = (True,xs)
char (x:xs) =
    case metachar (x:xs) of
        (True,_) -> (False,(x:xs))
        otherwise -> (True,xs)

metachar :: [Char] -> Match
metachar ('.':xs) = (True,xs)
metachar ('+':xs) = (True,xs)
metachar ('(':xs) = (True,xs)
metachar (')':xs) = (True,xs)
metachar ('[':xs) = (True,xs)
metachar ('^':xs) = (True,xs)
metachar ('$':xs) = (True,xs)
metachar ('|':xs) = (True,xs)
metachar ('?':xs) = (True,xs)
metachar ('\\':xs) = (True,xs)
metachar input = (False, input)

letter :: [Char] -> Match
letter [] = (False, [])
letter (x:xs) = (isLetter x, xs)

number :: [Char] -> Match
number input = case digit input of
    (True,r1) -> let (t2,r2) = number r1 in
        (True, if t2 then r2 else r1)
    otherwise -> (False, input)

digit :: [Char] -> Match
digit [] = (False, [])
digit (x:xs) = (isDigit x, xs)