module Main.Config.Parsing (parseConfig) where

import qualified Data.Char
import qualified Data.Char as Char
import Data.Map ((!), (!?))
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Language.Impe.Logging
import qualified Main.Config.Grammar as Grammar
import Options.Applicative
import qualified Paths_impe
import Polysemy
import Polysemy.Embed
import Text.Printf (printf)

{-
# Parsing
-}

parseConfig :: Member (Embed IO) r => Sem r Grammar.Config
parseConfig = embed $ execParser config

{-
## Config
-}

config :: ParserInfo Grammar.Config
config =
  info
    ( helper
        <*> version
        <*> (Grammar.Config <$> mode <*> verbosity <*> source_filename)
    )
    (fullDesc <> progDesc "impe" <> header "the impe language")

mode :: Parser Grammar.Mode
mode =
  flag
    Grammar.Mode_Interpret
    Grammar.Mode_Interact
    (short 'i' <> long "interactive" <> help "interactive REPL")

version :: Parser (a -> a)
version =
  infoOption
    (unwords [showVersion Paths_impe.version, $(gitHash)])
    (long "version" <> help "show version")

verbosity :: Parser Grammar.Verbosity
verbosity = do
  option
    parseVerbosity
    ( metavar "VERBOSITY"
        <> short 'v'
        <> long "verbosity"
        <> value (Grammar.verbosities ! "normal")
        <> help "verbosity modes: debug, normal, quiet, silent, arrogant"
    ) ::
    Parser Grammar.Verbosity

parseVerbosity :: ReadM Grammar.Verbosity
parseVerbosity =
  eitherReader $
    ( \s ->
        case Grammar.verbosities !? s of
          Just vrb -> return vrb
          Nothing -> Left $ printf "Unrecognized verbosity `%s'" s
    )
      . Prelude.filter (not . Char.isSpace)

source_filename :: Parser (Maybe String)
source_filename =
  Just
    <$> ( strArgument
            ( metavar "INPUT"
                <> help "input filename"
            )
        )
    <|> pure Nothing
