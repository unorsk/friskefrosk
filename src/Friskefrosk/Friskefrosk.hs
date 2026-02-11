module Friskefrosk.Friskefrosk (Term (..), Atom (..), Rule (..)) where

import Data.ByteString (ByteString)
import Data.Text (Text)

data Term = Var Text | Lit ByteString
  deriving (Show, Eq)

data Atom = Atom {atomName :: Text, atomAnti :: Bool, atomTerms :: [Term]}
  deriving (Show, Eq)

data Rule = Rule {ruleHead :: [Atom], ruleBody :: [Atom]}
  deriving (Show, Eq)
