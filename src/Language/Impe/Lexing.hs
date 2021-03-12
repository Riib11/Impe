{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Language.Impe.Lexing where

import Control.Monad.Identity
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token

type LexStream = String

type LexState = ()

type LexMonad = Identity

language :: Token.GenLanguageDef LexStream LexState LexMonad
language =
  Token.LanguageDef
    { Token.commentStart = "/*",
      Token.commentEnd = "*/",
      Token.commentLine = "//",
      Token.nestedComments = False,
      Token.identStart = letter <|> oneOf specials,
      Token.identLetter = alphaNum <|> oneOf specials,
      Token.opStart = choice [], -- oneOf "+-*/&|",
      Token.opLetter = choice [], -- oneOf "+-*/&|",
      Token.reservedNames = ["return"],
      Token.reservedOpNames = [], -- ["+", "-", "*", "/", "&", "|"],
      Token.caseSensitive = True
    }
  where
    specials = "~!@#$%^&*<>-=_+?/"

tokenParser :: Token.TokenParser LexState
tokenParser = Token.makeTokenParser language

braces = Token.braces tokenParser

identifier = Token.identifier tokenParser

colon = Token.colon tokenParser

semi = Token.semi tokenParser

parens = Token.parens tokenParser

commaSep = Token.commaSep tokenParser

symbol = Token.symbol tokenParser

integer = Token.integer tokenParser

natural = Token.natural tokenParser

stringLiteral = Token.stringLiteral tokenParser

--

eq = symbol "="

arrow = symbol "->"
