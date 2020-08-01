module Definitions where

type Input = [Char]

data ParseNode
  = Token [Char]
  | Letter
  | Digit
  | Number
  | Metachar
  | Char
  | Escape
  | EscapedChar
  | SetChar
  | Name
  | QuantifierH
  deriving (Show)

data ParseTree
  = Empty
  | Node ParseNode Matched [ParseTree]
  deriving (Show)

type Matched = [Char]

type Rest = [Char]

data Match = Match Matched Rest ParseTree deriving (Show)

type Matcher = Input -> Maybe Match

type MatcherChain = Input -> Maybe [Match]
