{-# language OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports #-}
module Algebra.Graph.IO.Internal.Megaparsec where

import Control.Applicative hiding (many, some)
import Data.Char (isAlpha, isSpace)
import Data.Void (Void)

-- megaparsec
import Text.Megaparsec (Parsec, parseTest, satisfy, (<?>))
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L
-- parser-combinators
import Control.Monad.Combinators (many, some, between)
-- text
import Data.Text (Text)

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- space consumer
sc :: Parser ()
sc = L.space
     space1
     (L.skipLineComment "//")
     (L.skipBlockComment "/*" "*/")

anyString :: Parser String
anyString = many (satisfy isAlpha)
