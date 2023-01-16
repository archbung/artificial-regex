{-# LANGUAGE DeriveGeneric #-}
module Regex (Regex(..), parse, solve, eval, simplify) where

import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Data.Void
import Data.List (nub, sortOn)
import Data.Maybe (fromJust)
import Test.QuickCheck
import GHC.Generics
import Generic.Random

data Regex
  = Atom Char 
  | And [Regex]
  | Or [Regex]
  deriving (Eq, Ord, Generic)

instance Show Regex where
  show (Atom c) = [c]
  show (And rs) = "(" <> concatMap show rs <> ")"
  show (Or rs)  = "[" <> concatMap show rs <> "]"

-- TODO: need to properly divide the size
instance Arbitrary Regex where
  arbitrary = genericArbitraryRec (1 % 1 % 1 % ()) 
    `withBaseCase` (Atom <$> arbitrary)

type Parser = Parsec Void String

pRegex :: Parser Regex
pRegex = Atom <$> lowerChar
  <|> And <$> (char '(' *> many pRegex <* char ')')
  <|> Or <$> (char '[' *> many pRegex <* char ']')

parse :: String -> Regex
parse = fromJust . parseMaybe pRegex

eval :: Regex -> [String]
eval (Atom c) = [[c]]
eval (Or rs)  = nub $ concatMap eval rs
eval (And rs) = foldr ((\as zs -> [ a <> z | a <- as, z <- zs]) . eval) [""] rs

simplify :: Regex -> Regex
simplify (Atom c) = Atom c
simplify (Or [r]) = simplify r
simplify (Or rs)  = Or $ nub $ foldr (op . simplify) [] rs
  where
    op (Or args) z = args <> z
    op a z = a:z
simplify (And [r]) = simplify r
simplify (And rs) = And $ foldr (op . simplify) [] rs
  where
    op (And args) z = args <> z
    op a z = a:z

solve :: String -> [(Int, String, String)]
solve = sortOn (\(x,_,_) -> x) . map ((\r -> (length (eval r), show r, show (simplify r))) . parse) . lines
