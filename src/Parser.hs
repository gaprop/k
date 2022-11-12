module Parser where

import AST
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number

parseExpr :: Parser Expr
parseExpr = fmap NoBlock parseExprWithoutBlock

parseExprWithoutBlock :: Parser ExprWithoutBlock
parseExprWithoutBlock = (fmap Operator parseOperExpr) <|> (fmap Literal parseLitExpr)


-- LiteralExpr

parseLitExpr :: Parser LiteralExpr
parseLitExpr = parseCharLit
           <|> parseStringLit
           <|> parseBoolLit
           <|> parseIntLit
           <|> parseFloatLit

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
parseIntLit = num >>= return . IntegerLiteral
  -- where
    -- withSign = do
      -- sign <- char '-'
      -- num <- noSign
      -- return $ sign : num
    -- num = do
      -- rest <- sepBy1 decimal (char '_')
      -- return $ concat (map show rest)

parseFloatLit :: Parser LiteralExpr
parseFloatLit = do
  first <- num :: Parser Int
  period <- char '.'
  rest <- num :: Parser Int
  return . FloatLiteral . read $ concat [show first, [period], show rest]

num :: Read a => Parser a
num = do
  rest <- sepBy1 decimal (char '_')
  return . read $ concat (map show rest)
