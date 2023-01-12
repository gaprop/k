module Parser where

import AST
import Stmt
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number

parseExpr :: Parser Expr
parseExpr = fmap NoBlock parseExprWithoutBlock

parseExprWithoutBlock :: Parser ExprWithoutBlock
parseExprWithoutBlock =  
                         try (fmap Operator parseOperExpr )
                     <|> try (fmap Literal parseLitExpr)


parseFunc :: Parser ExprWithoutBlock
parseFunc = do
  funcName <- lexeme $ parseIdent
  idents <- between (lexeme $ string "(") (lexeme $ string ")") $ sepBy (lexeme parseFuncParam) (lexeme $ char ',')
  lexeme $ string "=>"
  stmts <- between (lexeme $ string "{") (lexeme $ string "}") $ many parseStmt
  return $ Function funcName idents stmts

parseFuncParam :: Parser FuncParams
parseFuncParam = do
  ident <- lexeme $ parseIdent
  type' <- optionMaybe (do lexeme $ char ':'; parseType)
  return (ident, type')

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
  

---- OperatorExpr
parseOperExpr :: Parser OperatorExpr
parseOperExpr =  fmap Negation parseNegExpr
             <|> fmap ArithmeticOrLogical parseArithOrLogExpr
             <|> fmap Comparison parseComparisonExpr
             <|> fmap LazyBoolean parseLazyBoolExpr
             -- <|> fmap Assi

-- NegationExpr
parseNegExpr :: Parser NegExpr
parseNegExpr =  do char '-'; e <- parseExpr; return $ Negative e
            <|> do char '!'; e <- parseExpr; return $ Not e

parseWithOper :: Parser (Expr -> Expr -> a) -> Parser a
parseWithOper oper = do
  e1 <- fmap NoBlock $ fmap Literal parseLitExpr
  op <- oper
  e2 <- parseExpr
  return $ op e1 e2

-- ArithmeticOrLogicalExpr
parseArithOrLogExpr :: Parser ArithmeticOrLogicalExpr
parseArithOrLogExpr = parseWithOper oper

oper :: Parser (Expr -> Expr -> ArithmeticOrLogicalExpr)
oper =
      do char '+'; return Plus
  <|> do char '-'; return Minus
  <|> do char '*'; return Times
  <|> do char '/'; return Div
  <|> do char '%'; return Modulo
  <|> do char '&'; return ByteAnd
  <|> do char '^'; return ByteXor
  <|> do string "<<"; return LeftShift
  <|> do string ">>"; return LeftShift

-- ComparisonExpr
parseComparisonExpr :: Parser ComparisonExpr
parseComparisonExpr = parseWithOper comp

comp :: Parser (Expr -> Expr -> ComparisonExpr)
comp =
      do string "=="; return Equal
  <|> do string "!="; return NotEqual
  <|> do char '<'; return Less
  <|> do char '>'; return Greater
  <|> do string "<="; return LessEq
  <|> do string ">="; return GreaterEq

-- LazyBooleanExpr
parseLazyBoolExpr :: Parser LazyBooleanExpr
parseLazyBoolExpr = parseWithOper lazyComp

lazyComp :: Parser (Expr -> Expr -> LazyBooleanExpr)
lazyComp =
      do string "||"; return Or
  <|> do string "&&"; return And

parseAssignExpr :: Parser AssignmentExpr
parseAssignExpr = parseWithOper assign

assign :: Parser (Expr -> Expr -> AssignmentExpr)
assign = do char '='; return Assign

parseCompAssignExpr :: Parser CompAssignmentExpr
parseCompAssignExpr = parseWithOper compAss

compAss :: Parser (Expr -> Expr -> CompAssignmentExpr)
compAss =
      do string "+="; return CompPlus
  <|> do string "-="; return CompMinus
  <|> do string "*="; return CompTimes
  <|> do string "/="; return CompDiv
  <|> do string "%="; return CompModulo
  <|> do string "&="; return CompByteAnd
  <|> do string "^="; return CompByteXor
  <|> do string "<<="; return CompLeftShift
  <|> do string ">>="; return CompLeftShift
