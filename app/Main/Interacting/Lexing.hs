{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main.Interacting.Lexing where

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
      Token.identStart = alphaNum <|> char ':',
      Token.identLetter = alphaNum <|> char ':',
      Token.opStart = choice [],
      Token.opLetter = choice [],
      Token.reservedNames = [],
      Token.reservedOpNames = [],
      Token.caseSensitive = True
    }

tokenParser :: Token.TokenParser LexState
tokenParser = Token.makeTokenParser language

braces = Token.braces tokenParser

identifier = Token.identifier tokenParser

colon = Token.colon tokenParser

semi = Token.semi tokenParser

parens = Token.parens tokenParser

commaSep = Token.commaSep tokenParser

symbol = Token.symbol tokenParser
