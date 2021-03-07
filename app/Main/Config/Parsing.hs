module Main.Config.Parsing (parseConfig) where

import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Language.Impe.Logging
import qualified Main.Config.Grammar as Grammar
import Options.Applicative
import qualified Paths_impe
import Polysemy
import Polysemy.Embed

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
  vrb <-
    strOption
      ( metavar "VERBOSITY"
          <> short 'v'
          <> value ""
          <> help "verbosity levels: TODO"
      ) ::
      Parser String
  return $ Grammar.Verbosity [Tag_Output]

source_filename :: Parser (Maybe String)
source_filename =
  Just
    <$> ( strArgument
            ( metavar "INPUT"
                <> help "input filename"
            )
        )
    <|> pure Nothing
