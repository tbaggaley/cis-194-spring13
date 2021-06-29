module SExpr where

import           AParser
import           Control.Applicative
import           Data.Char


-- Ex 1.

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

-- Ex 2.

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

-- Allow a few more chars so we can have symbol function names like in lisp
allowedIdentifierChar :: Char -> Bool
allowedIdentifierChar x = isPrint x && not (isSpace x) && x /= '(' && x /= ')'

identifierChar :: Parser Char
identifierChar = satisfy allowedIdentifierChar

ident :: Parser String
ident = (++) <$> oneOrMore identifierChar <*> zeroOrMore identifierChar

-- Ex 3.

type Ident = String

data Atom = N Integer | I Ident
  deriving Show

data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseAtom :: Parser Atom
parseAtom = (N <$> posInt) <|> (I <$> ident)

parseSExpr :: Parser SExpr
parseSExpr =
  spaces
    *> (   (A <$> parseAtom)
       <|> (char '(' *> (Comb <$> oneOrMore parseSExpr) <* char ')')
       )
    <* spaces
