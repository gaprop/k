module Stmt where

import AST
import Parser
import Text.ParserCombinators.Parsec

lexeme :: Parser a -> Parser a
lexeme p = do
  p' <- p
  spaces
  skipMany parseComment
  spaces
  return p'

parseComment :: Parser ()
parseComment = do
  string "//"
  s <- many $ noneOf "\n"
  char '\n' <|> (try eof >> return '_')
  return ()

-- FIXME: This should be tested
parseIdent :: Parser String
parseIdent = do
  start <- letter
  rest <- many $ (letter <|> digit <|> char '_')
  return $ start : rest

parseStmt :: Parser Stmt
parseStmt =  parseLetStmt 
         <|> parseItem

parseItem :: Parser Stmt
parseItem = parseFunc

parseLetStmt :: Parser Stmt
parseLetStmt = do
  lexeme $ string "let"
  ident <- lexeme $ parseIdent
  type' <- optionMaybe (do char ':'; parseType)
  lexeme $ char '='
  expr <- parseExpr
  lexeme $ char ';'
  return $ LetStmt ident expr

parseType :: Parser Type
parseType =  string "char"
         <|> string "string"
         <|> string "int" -- Change to u32 or whatever later
         <|> string "float" -- Change to f32 or whatever later
         <|> string "bool"
         <|> string "fun" -- FIXME: Tempoary

