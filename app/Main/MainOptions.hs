module Main.MainOptions where

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
import Polysemy.Fail as Fail
import Polysemy.Output as Output
import Polysemy.Reader as Reader
import Polysemy.State as State
import Text.ParserCombinators.Parsec (runParser)
import Text.Printf

{-
# MainOptions
-}

{-
## Data
-}

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
  | Phase_Interpreting
  deriving (Eq)

instance Show Phase where
  show = \case
    Phase_Reading -> "reading"
    Phase_Parsing -> "parsing"
    Phase_Typechecking -> "typechecking"
    Phase_Executing -> "executing"
    Phase_Interpreting -> "interpreting"

phase_symbols =
  [ ('c', Phase_Reading),
    ('p', Phase_Parsing),
    ('t', Phase_Typechecking),
    ('e', Phase_Executing),
    ('i', Phase_Interpreting)
  ]

data Mode
  = Mode_Interpret
  | Mode_Interactive
  deriving (Show)

{-
## Parsing
-}

parseMainOptions :: ParserInfo MainOptions
parseMainOptions =
  info
    ( helper
        <*> parseVersion
        <*> ( MainOptions
                <$> parseMode
                  <*> parseVerbosity
                  <*> parseInputFilename
            )
    )
    (fullDesc <> progDesc "impe" <> header "the impe language")
  where
    parseVersion :: Parser (a -> a)
    parseVersion =
      infoOption
        (unwords [showVersion version, $(gitHash)])
        (long "version" <> help "show version")

    parseMode :: Parser Mode
    parseMode =
      flag
        Mode_Interactive
        Mode_Interpret
        (short 'i' <> long "interactive" <> help "interactive REPL")

    parseVerbosity :: Parser [Phase]
    parseVerbosity = do
      vrb <-
        strOption
          ( short 'v'
              <> long "verbosity"
              <> metavar "VERBOSITY"
              <> value ""
              <> help "verbosity phases: (r)eading, (p)arsing, (t)ypechecking, (e)xecuting, (i)nterpreting"
          ) ::
          Parser String
      pure $
        mapMaybe
          (\(c, phase) -> if c `elem` vrb then Just phase else Nothing)
          phase_symbols

    parseInputFilename :: Parser (Maybe String)
    parseInputFilename =
      (Just <$> strArgument (metavar "INPUT" <> help "input filename"))
        <|> pure Nothing
