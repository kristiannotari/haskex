module Helper where

import Data.List (stripPrefix)
import Data.Maybe (isNothing)
import Definitions
import Prelude hiding (until)

-- applying
on :: Matcher -> Input -> Maybe Match
on f i = f i

-- naming
as :: (ParseNode -> a) -> ParseNode -> a
as f node = f node

mergedAs :: MatcherChain -> ParseNode -> Matcher
mergedAs f = go f
  where
    go f node input = case f input of
      Just matches -> Just $ merge matches `as` node
      otherwise -> Nothing

-- logic
or :: Matcher -> Matcher -> Matcher
or f1 f2 = go f1 f2
  where
    go f1 f2 input =
      let r1 = f1 input in if isNothing r1 then f2 input else r1

and :: Matcher -> Matcher -> (ParseNode -> Matcher)
and f1 f2 = go
  where
    go node input =
      case f1 input of
        Just (Match m r t) ->
          case f2 r of
            Just m2 -> Just $ merge [Match m r t, m2] `as` node
            otherwise -> Nothing
        otherwise -> Nothing

followedBy :: Matcher -> MatcherChain -> (ParseNode -> Matcher)
followedBy f1 f2 = go f1 f2
  where
    go f1 f2 node input =
      case f1 input of
        Just (Match m r t) ->
          case f2 r of
            Just matches -> Just $ merge ((Match m r t) : matches) `as` node
            Nothing -> Nothing
        Nothing -> Nothing

except :: Matcher -> Matcher -> Matcher
except f e = go f e
  where
    go f e input = case e input of
      Just _ -> Nothing
      otherwise -> f input

-- matching
matchCharIf :: (Char -> Bool) -> (ParseNode -> Matcher)
matchCharIf f = go f
  where
    go _ _ [] = Nothing
    go f node (x : xs) =
      if f x then Just $ Match [x] xs $ Node node [x] [] else Nothing

anyChar :: ParseNode -> Matcher
anyChar node = go
  where
    go [] = Nothing
    go (x : xs) = Just $ Match [x] xs $ Node node [x] []

exactly :: [Char] -> (ParseNode -> Matcher)
exactly m = go m
  where
    go m node input = case stripPrefix m input of
      Just r -> Just $ Match m r $ Node node m []
      otherwise -> Nothing

token :: [Char] -> Matcher
token t = exactly t `as` Token t

-- repetitions
oneOrMore :: Matcher -> MatcherChain
oneOrMore f = go f
  where
    go f input = case until f input of
      [] -> Nothing
      matches -> Just matches

zeroOrMore :: Matcher -> MatcherChain
zeroOrMore f = go f
  where
    go f input = case oneOrMore f input of
      Just matches -> Just matches
      otherwise -> Just []

-- utils
until :: Matcher -> Input -> [Match]
until f input = go f [] input
  where
    go f matches rest = case f rest of
      Just (Match m r t) -> go f (matches ++ [Match m r t]) r
      Nothing -> matches

merge :: [Match] -> ParseNode -> Match
merge matches node = go matches ([], [], []) node
  where
    go [] (m, r, t) node = Match m r $ Node node m t
    go (Match m r t : ms) (matched, rest, trees) node = go ms (matched ++ m, r, trees ++ [t]) node