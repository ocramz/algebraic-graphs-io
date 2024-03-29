{-# language OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports #-}
module Algebra.Graph.IO.Internal.Megaparsec (Parser, ParserT, ParseE,
                                            -- ** Internal
                                            lexeme, symbol, anyString, alphaNum
                                            ) where

import Control.Applicative hiding (many, some)
import Data.Char (isAlpha, isSpace, isAlphaNum)
import Data.Void (Void)

-- megaparsec
import Text.Megaparsec (Parsec, ParsecT, parseTest, satisfy, (<?>))
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Error (ParseErrorBundle)
import qualified Text.Megaparsec.Char.Lexer as L
-- parser-combinators
import Control.Monad.Combinators (many, some, between)
-- text
import Data.Text (Text)

type Parser = Parsec Void Text

type ParserT = ParsecT Void Text

type ParseE = ParseErrorBundle Text Void

lexeme :: Parser a -- ^ disregard any whitespace around this parser
       -> Parser a
lexeme = L.lexeme sc

-- | Match a string
symbol :: Text
       -> Parser Text
symbol = L.symbol sc

-- | space consumer
sc :: Parser ()
sc = L.space
     space1
     (L.skipLineComment "//")
     (L.skipBlockComment "/*" "*/")

anyString, alphaNum :: Parser String
anyString = many (satisfy isAlpha)

alphaNum = many (satisfy $ \c -> isAlphaNum c || c `elem` ['-', '_']) <?> "alphanumeric string or -"
