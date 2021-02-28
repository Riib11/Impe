{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module MainOptions where

import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Language.Impe.Executing
import Language.Impe.Grammar
import Language.Impe.Parsing
import Language.Impe.Typechecking
import Options.Applicative
import Paths_impe (version)
import Polysemy
import Polysemy.Error as Error
import Polysemy.Output as Output
import Polysemy.State as State
import Text.ParserCombinators.Parsec (runParser)
import Text.Printf

data MainOptions = MainOptions
  { mode :: Mode,
    verbosity :: [Phase],
    input_filename :: Maybe String
  }
  deriving (Show)

data Phase
  = Phase_Reading
  | Phase_Parsing
  | Phase_Typechecking
  | Phase_Executing
  deriving (Eq, Show)

phase_symbols =
  [ ('c', Phase_Reading),
    ('p', Phase_Parsing),
    ('t', Phase_Typechecking),
    ('e', Phase_Executing)
  ]

data Mode
  = Mode_Interpret
  | Mode_Interactive
  deriving (Show)

makeLenses ''MainOptions

parseMainOptions :: ParserInfo MainOptions
parseMainOptions =
  info
    (helper <*> parseVersion <*> parseProgramOptions)
    (fullDesc <> progDesc "impe" <> header "impe - an imperative, interpreted, simple, extendible language")

parseVersion :: Parser (a -> a)
parseVersion =
  infoOption
    (unwords [showVersion version, $(gitHash)])
    (long "version" <> help "show version")

parseProgramOptions :: Parser MainOptions
parseProgramOptions = MainOptions <$> parseMode <*> parseVerbosity <*> parseInputFilename
  where
    parseMode :: Parser Mode
    parseMode =
      flag
        Mode_Interpret
        Mode_Interactive
        (short 'i' <> long "interactive" <> help "interactive REPL")
    parseVerbosity :: Parser [Phase]
    parseVerbosity = do
      vrb <- strOption (short 'v' <> long "verbosity" <> metavar "VERBOSITY" <> value "" <> help "verbosity phases: (r)eading, (p)arsing, (t)ypechecking, (e)xecuting") :: Parser String
      pure $ mapMaybe (\(c, phase) -> if c `elem` vrb then Just phase else Nothing) phase_symbols
    parseInputFilename :: Parser (Maybe String)
    parseInputFilename =
      (Just <$> strArgument (metavar "INPUT" <> help "input filename"))
        <|> pure Nothing
