module Parser where

import AST
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number

parseCharLit :: Parser LiteralExpr 
parseCharLit = do
  char '\''
  c <- noneOf "'"
  char '\''
  return $ CharLiteral c

parseStringLit :: Parser LiteralExpr
parseStringLit = do
  char '"'
  s <- many $ noneOf "\""
  char '"'
  return $ StringLiteral s

parseBoolLit :: Parser LiteralExpr
parseBoolLit =  do string "true"; return TrueVal
            <|> do string "false"; return FalseVal

parseIntLit :: Parser LiteralExpr
parseIntLit = (withSign <|> noSign) >>= return . IntegerLiteral . read
  where
    withSign = do
      sign <- char '-'
      num <- noSign
      return $ sign : num
    noSign = do
      rest <- sepBy1 decimal (char '_')
      return $ concat (map show rest)
