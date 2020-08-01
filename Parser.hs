module Parser where

import Data.Char (isDigit, isLetter)
import Definitions
import Helper
import Prelude hiding (and, not, or)

quantifierH :: Matcher
quantifierH input =
  exactly ['?'] `as` QuantifierH
    `or` (exactly ['+'] `as` QuantifierH)
    `or` (exactly ['*'] `as` QuantifierH)
    `or` ((token ['{'] `and` number) `as` QuantifierH)
    `on` input

name :: Matcher
name input =
  letter `followedBy` (zeroOrMore (letter `or` digit))
    `as` Name
      `on` input

setChar :: Matcher
setChar input =
  (anyChar `as` SetChar `except` (token [']']))
    `on` input

char :: Matcher
char input =
  anyChar `as` Char `except` metachar
    `or` escapedChar
    `on` input

escapedChar :: Matcher
escapedChar input =
  token ['\\'] `and` (anyChar `as` Char) `as` EscapedChar
    `on` input

metachar :: Matcher
metachar input =
  exactly ['.'] `as` Metachar
    `or` (exactly ['+'] `as` Metachar)
    `or` (exactly ['('] `as` Metachar)
    `or` (exactly [')'] `as` Metachar)
    `or` (exactly ['['] `as` Metachar)
    `or` (exactly ['$'] `as` Metachar)
    `or` (exactly ['?'] `as` Metachar)
    `or` (exactly "|" `as` Metachar)
    `or` (exactly ['\\'] `as` Metachar)
    `on` input

number :: Matcher
number input = oneOrMore digit `mergedAs` Number `on` input

digit :: Matcher
digit input =
  matchCharIf isDigit `as` Digit
    `on` input

letter :: Matcher
letter input =
  matchCharIf isLetter `as` Letter
    `on` input