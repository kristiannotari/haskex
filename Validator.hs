module Validator where

import Definitions
import Parser (regex)

isValid :: Input -> Bool
isValid input =
  case regex input of
    Just (_, [], _) -> True
    otherwise -> False