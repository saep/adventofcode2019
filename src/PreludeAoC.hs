module PreludeAoC
  ( module X
  , Parser
  , parseFile
  ) where

import RIO as X hiding (try, many, some)
import Text.Megaparsec as X
import Text.Megaparsec.Char as X

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
