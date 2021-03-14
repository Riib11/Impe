{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Language.Impe.Lexing
  ( whiteSpace,
    braces,
    identifier,
    colon,
    semi,
    parens,
    commaSep,
    symbol,
    integer,
    natural,
    stringLiteral,
    reserved,
    reservedOp,
    eq,
    arrow,
  )
where

import Control.Monad.Identity
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

type LexStream = String

type LexState = ()

type LexMonad = Identity

language :: Token.GenLanguageDef LexStream LexState LexMonad
language =
  emptyDef
    { Token.commentStart = "/*",
      Token.commentEnd = "*/",
      Token.commentLine = "//",
      Token.identStart = letter,
      Token.identLetter = alphaNum <|> oneOf ['_'],
      Token.reservedNames =
        [ "while",
          "do",
          "if",
          "then",
          "else",
          "return",
          "pass"
        ],
      Token.reservedOpNames =
        [ "<-",
          "&&",
          "||",
          "~",
          "+",
          "-",
          "*",
          "/",
          "^",
          "%",
          "=",
          ">",
          ">=",
          "<",
          "<=",
          "<>"
        ],
      Token.caseSensitive = True
    }

tokenParser :: Token.TokenParser LexState
tokenParser = Token.makeTokenParser language

whiteSpace = Token.whiteSpace tokenParser

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

reserved = Token.reserved tokenParser

reservedOp = Token.reservedOp tokenParser

--

eq = reserved "="

arrow = reserved "->"
