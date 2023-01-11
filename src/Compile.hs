module Compile where

import Control.Monad.Writer
import Control.Monad.Reader
import AST

type VEnv  = [Ident]
type FEnv = [Ident]

type Comp a = ReaderT (VEnv, FEnv) (Writer [String]) a

-- compile = undefined

compProgram :: Program -> Comp [String]
compProgram program = do 
  stmts <- mapM compStmt program
  return $ concat stmts

compExpr :: Expr -> Comp String
compExpr (NoBlock (Operator expr)) = compOperatorExpr expr

-- FIXME: Refactor with below
compOperatorExpr :: OperatorExpr -> Comp String
compOperatorExpr (ArithmeticOrLogical e) = compArithOrLogExpr e

compOperatorExpr (Negation (Negative e)) = do
  e' <- compExpr e
  return $ "-" ++ e'

compOperatorExpr (Negation (Not e)) = do
  e' <- compExpr e
  return $ "!" ++ e'

compLitExpr :: LiteralExpr -> Comp String
compLitExpr (CharLiteral c) = return [c]
compLitExpr (StringLiteral s) = return s
compLitExpr (ByteLiteral b) = return $ show b
compLitExpr (IntegerLiteral i) = return $ show i
compLitExpr (FloatLiteral i) = return $ show i
compLitExpr (TrueVal) = return "true"
compLitExpr (FalseVal) = return "false"

-- -- Operators
compArithOrLogExpr :: ArithmeticOrLogicalExpr -> Comp String
compArithOrLogExpr (Plus e1 e2)       = compOp e1 e2 "+"
compArithOrLogExpr (Minus e1 e2)      = compOp e1 e2 "-"
compArithOrLogExpr (Times e1 e2)      = compOp e1 e2 "*"
compArithOrLogExpr (Div e1 e2)        = compOp e1 e2 "/"
compArithOrLogExpr (Modulo e1 e2)     = compOp e1 e2 "%"
compArithOrLogExpr (ByteAnd e1 e2)    = compOp e1 e2 "&"
compArithOrLogExpr (ByteOr e1 e2)     = compOp e1 e2 "|"
compArithOrLogExpr (ByteXor e1 e2)    = compOp e1 e2 "^"
compArithOrLogExpr (LeftShift e1 e2)  = compOp e1 e2 "<<"
compArithOrLogExpr (RightShift e1 e2) = compOp e1 e2 ">>"

compComparisonExpr :: ComparisonExpr -> Comp String
compComparisonExpr (Equal e1 e2) = compOp e1 e2 "=="
compComparisonExpr (NotEqual e1 e2) = compOp e1 e2 "!="
compComparisonExpr (Less e1 e2) = compOp e1 e2 "<"
compComparisonExpr (LessEq e1 e2) = compOp e1 e2 "<="
compComparisonExpr (Greater e1 e2) = compOp e1 e2 ">"
compComparisonExpr (GreaterEq e1 e2) = compOp e1 e2 ">="

compLazyBooleanExpr :: LazyBooleanExpr -> Comp String
compLazyBooleanExpr (Or e1 e2) = compOp e1 e2 "||"
compLazyBooleanExpr (And e1 e2) = compOp e1 e2 "&&"

compOp :: Expr -> Expr -> String -> Comp String
compOp e1 e2 op = do
  e1' <- compExpr e1
  e2' <- compExpr e2
  return $ e1' ++ op ++ e2'



-- -- Statements
compStmt :: Stmt -> Comp [String]
compStmt (Function ident params block) = do
  code <- compBlock block
  let l1 = "func " ++ ident ++ "(" ++ foldl (\acc x -> if acc == "" then x else acc ++ "," ++ x) "" params ++ ")" ++ "{"
  let l2 = code
  let l3 = "}"
  return $ [l1] ++ l2 ++ [l3]

-- compStmt (LetStmt ident expr) =

compBlock :: [Stmt] -> Comp [String]
compBlock = fmap concat . mapM compStmt
