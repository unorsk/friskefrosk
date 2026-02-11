{-# LANGUAGE OverloadedStrings #-}

module Friskefrosk.Parse
  ( parse,
    ParseError,
  )
where

import Data.Bits (shiftR)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (isAlphaNum, isDigit)
import Data.Text (Text, length, unpack)
import Data.Text qualified as T
import Data.Void (Void)
import Data.Word (Word32)
import Friskefrosk.Friskefrosk (Atom (..), Rule (..), Term (..))
import Text.Megaparsec (ParseErrorBundle, Parsec, between, customFailure, empty, eof, many, option, runParser, sepBy, sepBy1, takeWhile1P, try, (<?>), (<|>))
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

type ParseError = ParseErrorBundle Text Void

parse :: Text -> Text -> Either ParseError [Rule]
parse source = runParser (sc *> many pRule <* eof) $ T.unpack source

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pRule :: Parser Rule
pRule = do
  h <- pAtoms
  b <- pTurnstile *> pAtoms <|> pure []
  _ <- symbol "."
  pure (Rule h b)
  where
    pTurnstile = symbol ":-"
    pAtoms = pAtom `sepBy1` symbol ","

pAtom :: Parser Atom
pAtom = do
  anti <- option False (True <$ char '!')
  name <- lexeme pName <?> "relation name"
  terms <- parens (pTerm `sepBy` symbol ",")
  pure (Atom name anti terms)

pName :: Parser Text
pName = takeWhile1P (Just "identifier character") isIdentChar
  where
    isIdentChar c = isAlphaNum c || c == '_'

pTerm :: Parser Term
pTerm = lexeme (pHexLit <|> pIntLit <|> pVar) <?> "term"

pVar :: Parser Term
pVar = do
  name <- pName
  if name == "_"
    then customFailure (undefined :: Void) -- or: fail "wildcards not yet supported"
    else pure (Var name)

pHexLit :: Parser Term
pHexLit = do
  _ <- string "0x"
  digits <- takeWhile1P (Just "hex digit") isHexDigit
  case hexToBytes digits of
    Just bs -> pure (Lit bs)
    Nothing -> fail "hex literal must have even number of digits"
  where
    isHexDigit c = isDigit c || c `elem` ("abcdefABCDEF" :: [Char])

pIntLit :: Parser Term
pIntLit = try $ do
  n <- L.decimal :: Parser Word32
  pure (Lit (word32BE n))

word32BE :: Word32 -> ByteString
word32BE w =
  BS.pack
    [ fromIntegral (w `shiftR` 24),
      fromIntegral (w `shiftR` 16),
      fromIntegral (w `shiftR` 8),
      fromIntegral w
    ]

hexToBytes :: Text -> Maybe ByteString
hexToBytes t
  | odd (Data.Text.length t) = Nothing
  | otherwise = Just . BS.pack $ go (Data.Text.unpack t)
  where
    go [] = []
    go (a : b : rest) = (hexVal a * 16 + hexVal b) : go rest
    go _ = []
    hexVal c
      | isDigit c = fromIntegral (fromEnum c - fromEnum '0')
      | c >= 'a' && c <= 'f' = fromIntegral (fromEnum c - fromEnum 'a' + 10)
      | c >= 'A' && c <= 'F' = fromIntegral (fromEnum c - fromEnum 'A' + 10)
      | otherwise = 0