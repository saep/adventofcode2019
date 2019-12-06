module PreludeAoC
  ( module X
  , Parser
  , parseFile
  ) where

import RIO as X hiding (try, many, some, Reader, ask, runReader, local, asks)
import Text.Megaparsec as X hiding (State)
import Text.Megaparsec.Char as X
import Polysemy as X
import Polysemy.Reader as X
import Polysemy.State as X

type Parser a = Parsec Void Text a

parseFile
  :: FilePath
  -> Parser a
  -> IO a
parseFile file parser = do
  input <- readFileUtf8 file
  case runParser parser file input of
    Left e ->
      error $ errorBundlePretty e
    Right masses ->
      pure masses
