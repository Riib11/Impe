module Main.Output where

import Main.Config.Grammar
import Polysemy
import Polysemy.Reader

-- if configured output file, write output to it (overwrites file);
-- otherwise, print output to console
writeOutput ::
  ( Member (Embed IO) r,
    Member (Reader Config) r
  ) =>
  String ->
  Sem r ()
writeOutput out = do
  asks output_filename >>= \case
    Just out_fn -> embed $ writeFile out_fn (out ++ "\n")
    Nothing -> embed $ putStr (out ++ "\n")

-- if configured output file, append output to it (does not overwrite file);
-- otherwise, print output to console
writeOutputAppended ::
  ( Member (Embed IO) r,
    Member (Reader Config) r
  ) =>
  String ->
  Sem r ()
writeOutputAppended out = do
  asks output_filename >>= \case
    Just out_fn -> embed $ appendFile out_fn out
    Nothing -> embed $ putStr out
