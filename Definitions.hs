module Definitions where

type Input = [Char]

data QuantifierType
  = ZeroOrMore
  | OneOrMore
  | ZeroOrOne
  | FromTo Int Int
  | From Int
  | Exactly Int
  deriving (Show)

data SetItemType
  = Range String String
  | SetChar String
  deriving (Show)

data ParseNode
  = Number Int
  | Digit Char
  | Letter Char
  | Metachar Char
  | Char Char
  | EscapedChar Char
  | KeyChar Char
  | Name String
  | Quantifier QuantifierType
  | LazyQuantifier
  | SetItem SetItemType
  | SetItems
  | Set
  | Group
  | Dot
  | Start
  | End
  | Regex
  | OrRegex
  | SequenceRegex
  deriving (Show)

data ParseTree
  = Empty
  | Node ParseNode [ParseTree]
  deriving (Show)

type Matched = [Char]

type Rest = [Char]

type Match = (Matched, Rest, ParseTree)
