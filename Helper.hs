module Helper where

import Definitions
import Data.List (stripPrefix)

exactlyMatchWith :: ParseNode -> [Char] -> Input -> Maybe Match
exactlyMatchWith _ _ [] = Nothing
exactlyMatchWith n m input =
  case stripPrefix m input of
    Just r -> Just (m, r, Node n [])
    otherwise -> Nothing

sequenceMatchWith :: ParseNode -> [(Input -> Maybe Match)] -> Input -> Maybe Match
sequenceMatchWith n matchers input =
  case _sequenceMatch [] input [] matchers of
    Just (m, r, trees) -> Just (m, r, Node n trees)
    otherwise -> Nothing
  where
    _sequenceMatch matched rest trees [] = Just (matched, rest, trees)
    _sequenceMatch matched rest trees (f : fs) =
      case f rest of
        Just (m, r, t) -> _sequenceMatch (matched ++ m) r (trees ++ [t]) fs
        otherwise -> Nothing

firstMatch :: [(Input -> Maybe Match)] -> Input -> Maybe Match
firstMatch [] _ = Nothing
firstMatch (f : fs) input =
  case f input of
    Just (m, r, t) -> Just (m, r, t)
    otherwise -> firstMatch fs input
