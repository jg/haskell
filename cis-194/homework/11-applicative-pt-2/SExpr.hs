{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

-- | Takes a parser as input and runs it consecutively as many times as possible
-- >>> runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH"
-- Just ("ABC","dEfgH")
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (char ' ')

-- | Ident
-- >>> runParser ident "foobar baz"
-- Just ("foobar"," baz")
-- >>> runParser ident "foo33fA"
-- Just ("foo33fA","")
-- >>> runParser ident "2bad"
-- Nothing
-- >>> runParser ident ""
-- Nothing
ident :: Parser String
ident = (:) <$> satisfy (isAlpha) <*> zeroOrMore (satisfy (isAlphaNum))

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

-- | Parses SExprs
-- >>> runParser parseSExpr "5"
-- Just (A (N 5),"")
-- >>> runParser parseSExpr "(bar (foo) 3 5 874)"
-- Just (Comb [A (I "bar"),Comb [A (I "foo")],A (N 3),A (N 5),A (N 874)],"")
-- >>> runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)"
-- Just (Comb [Comb [Comb [A (I "lambda"),A (I "x"),Comb [A (I "lambda"),A (I "y"),Comb [A (I "plus"),A (I "x"),A (I "y")]]],A (N 3)],A (N 5)],"")
-- >>> runParser parseSExpr "( lots of ( spaces in ) this ( one ) )"
-- Just (Comb [A (I "lots"),A (I "of"),Comb [A (I "spaces"),A (I "in")],A (I "this"),Comb [A (I "one")]],"")
parseSExpr :: Parser SExpr
parseSExpr = A <$> (spaces *> parseAtom)
             <|>
             spaces *> char '(' *> (Comb <$> (zeroOrMore parseSExpr)) <* spaces <* char ')'

parseAtom :: Parser Atom
parseAtom = N <$> posInt
            <|>
            I <$> ident
