module OperatorExpr(parseOperExpr) where

---- OperatorExpr
parseOperExpr :: Parser OperatorExpr
parseOperExpr = 
  fmap Negation parseNegExpr

-- NegationExpr
parseNegExpr :: Parser NegExpr
parseNegExpr =  do char '-'; e <- parseExpr; return $ Negative e
            <|> do char '!'; e <- parseExpr; return $ Not e

parseWithOper :: Parser (Expr -> Expr -> a) -> Parser a
parseWithOper oper = do
  op <- oper
  e1 <- parseExpr
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
