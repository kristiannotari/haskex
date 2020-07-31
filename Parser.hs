module Parser where

import Data.Char (isDigit, isLetter)
import Helper
import Definitions

regex :: Input -> Maybe Match
regex [] = Just ([], [], Empty)
regex input =
  firstMatch
    [ sequenceMatchWith Regex [regexHL, regexTS],
      regexHL
    ]
    input

regexHL :: Input -> Maybe Match
regexHL input =
  firstMatch
    [ sequenceMatchWith Regex [regexH, quantifier],
      regexH,
      regexL
    ]
    input

regexH :: Input -> Maybe Match
regexH input =
  firstMatch
    [ group,
      set,
      char,
      exactlyMatchWith Dot ['.']
    ]
    input

regexL :: Input -> Maybe Match
regexL ('^' : xs) = Just (['^'], xs, Node Start [])
regexL ('$' : xs) = Just (['$'], xs, Node Start [])
regexL input = Nothing

regexTS :: Input -> Maybe Match
regexTS input =
  firstMatch
    [ sequenceMatchWith SequenceRegex [regexT, regexTS],
      regexT
    ]
    input

regexT :: Input -> Maybe Match
regexT input =
  firstMatch
    [ regexHL,
      sequenceMatchWith OrRegex [exactlyMatchWith (KeyChar '|') "|", regexHL]
    ]
    input

group :: Input -> Maybe Match
group ('(' : xs) =
  case xs of
    ('?' : r1) ->
      case r1 of
        ('<' : r2) ->
          case name r2 of
            Just (m, ('>' : r3), t) -> _group $ go (['(', '?', '<'] ++ m) r3 [t]
            otherwise -> Nothing
        (':' : r2) -> _group $ go ['(', '?', ':'] r2 []
        ('=' : r2) -> _group $ go ['(', '?', '='] r2 []
        ('!' : r2) -> _group $ go ['(', '?', '!'] r2 []
        otherwise -> _group $ go ['(', '?'] r1 []
    otherwise -> _group $ go ['('] xs []
  where
    _group match = case match of
      Just (m, r, trees) -> Just (m, r, Node Group trees)
      otherwise -> Nothing
    go matched (')' : rest) trees = Just (matched, rest, trees)
    go matched rest trees =
      case regex rest of
        Just (m, r, t) -> go (matched ++ m) r (trees ++ [t])
        otherwise -> Nothing
group input = Nothing

set :: Input -> Maybe Match
set ('[' : xs) =
  case xs of
    ('^' : xss) -> _set $ go ['[', '^'] xss []
    otherwise -> _set $ go ['['] xs []
  where
    _set match = case match of
      Just (m, r, trees) -> Just (m, r, Node Set trees)
      otherwise -> Nothing
    go matched (']' : rest) trees = Just (matched, rest, trees)
    go matched rest trees = case setItems rest of
      Just (m, r, t) -> go (matched ++ m) r (trees ++ [t])
      otherwise -> Nothing
set input = Nothing

setItems :: Input -> Maybe Match
setItems input =
  case setItem input of
    Just (m1, r1, t1) ->
      case setItems r1 of
        Just (m2, r2, t2) -> Just (m1 ++ m2, r2, Node SetItems [t1, t2])
        otherwise -> Just (m1, r1, Node SetItems [t1])
    otherwise -> Nothing

setItem :: Input -> Maybe Match
setItem input =
  case setChar input of
    Just (m1, ('-' : r1), t1) ->
      case setChar r1 of
        Just (m2, r2, t2) -> Just (m1 ++ ['-'] ++ m2, r2, Node (SetItem $ Range m1 m2) [t1, t2])
        Nothing -> Just (m1, r1, Node (SetItem $ SetChar m1) [t1])
    Just (m1, r1, t1) -> Just (m1, r1, Node (SetItem $ SetChar m1) [t1])
    otherwise -> Nothing

quantifier :: Input -> Maybe Match
quantifier input =
  firstMatch
    [ sequenceMatchWith
        LazyQuantifier
        [quantifierH, exactlyMatchWith (KeyChar '?') ['?']],
      quantifierH
    ]
    input

quantifierH :: Input -> Maybe Match
quantifierH ('?' : xs) = Just (['?'], xs, Node (Quantifier ZeroOrOne) [])
quantifierH ('+' : xs) = Just (['+'], xs, Node (Quantifier OneOrMore) [])
quantifierH ('*' : xs) = Just (['*'], xs, Node (Quantifier ZeroOrMore) [])
quantifierH ('{' : xs) =
  case number xs of
    Just (m, ('}' : r), t) -> Just (['{'] ++ m ++ ['}'], r, Node (Quantifier $ Exactly $ read m) [t])
    Just (m1, (',' : r1), t1) ->
      case number r1 of
        Just (m2, ('}' : r2), t2) -> Just (['{'] ++ m1 ++ m2 ++ ['}'], r2, Node (Quantifier $ FromTo (read m1) (read m2)) [t1, t2])
        Nothing ->
          case r1 of
            ('}' : r2) -> Just (['{'] ++ m1 ++ ['}'], r2, Node (Quantifier $ From (read m1)) [])
            otherwise -> Nothing
    otherwise -> Nothing
quantifierH input = Nothing

name :: Input -> Maybe Match
name input =
  case _name [] input [] of
    ([], input, _) -> Nothing
    (m, r, trees) -> Just (m, r, Node (Name m) trees)
  where
    _name matched input trees = case firstMatch [letter, digit] input of
      Just (m, r, t) -> _name (matched ++ m) r (trees ++ [t])
      otherwise -> (matched, input, trees)

setChar :: Input -> Maybe Match
setChar [] = Nothing
setChar (']' : xs) = Nothing
setChar (x : xs) = Just ([x], xs, Node (Char x) [])

char :: Input -> Maybe Match
char [] = Nothing
char ('\\' : x : xs) = Just (['\\', x], xs, Node (EscapedChar x) [])
char (x : xs) =
  case metachar (x : xs) of
    Just (m, r, t) -> Nothing
    otherwise -> Just ([x], xs, Node (Char x) [])

metachar :: Input -> Maybe Match
metachar ('.' : xs) = Just (['.'], xs, Node (Metachar '.') [])
metachar ('+' : xs) = Just (['+'], xs, Node (Metachar '+') [])
metachar ('(' : xs) = Just (['('], xs, Node (Metachar '(') [])
metachar (')' : xs) = Just ([')'], xs, Node (Metachar ')') [])
metachar ('[' : xs) = Just (['['], xs, Node (Metachar '[') [])
metachar ('^' : xs) = Just (['^'], xs, Node (Metachar '^') [])
metachar ('$' : xs) = Just (['$'], xs, Node (Metachar '$') [])
metachar ('|' : xs) = Just ("|", xs, Node (Metachar '|') [])
metachar ('?' : xs) = Just (['?'], xs, Node (Metachar '?') [])
metachar ('\\' : xs) = Just (['\\'], xs, Node (Metachar '\\') [])
metachar input = Nothing

letter :: Input -> Maybe Match
letter [] = Nothing
letter (x : xs) =
  if isLetter x then Just ([x], xs, Node (Letter x) []) else Nothing

number :: Input -> Maybe Match
number input =
  case _number [] input [] of
    ([], input, _) -> Nothing
    (m, r, trees) -> Just (m, r, Node (Number $ read m) trees)
  where
    _number matched input trees = case digit input of
      Just (m, rest, t) -> _number (matched ++ m) rest (trees ++ [t])
      otherwise -> (matched, input, trees)

digit :: Input -> Maybe Match
digit [] = Nothing
digit (x : xs) =
  if isDigit x then Just ([x], xs, Node (Digit x) []) else Nothing
