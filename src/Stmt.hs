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

parseFunc :: Parser Stmt
parseFunc = do
  lexeme $ string "fn"
  funcName <- lexeme $ parseIdent
  idents <- between (lexeme $ string "(") (lexeme $ string ")") $ sepBy (lexeme parseIdent) (lexeme $ char ',')
  stmts <- between (lexeme $ string "{") (lexeme $ string "}") $ many parseStmt
  return $ Function funcName idents stmts

parseLetStmt :: Parser Stmt
parseLetStmt = do
  lexeme $ string "let"
  ident <- lexeme $ parseIdent
  lexeme $ char '='
  expr <- parseExpr
  lexeme $ char ';'
  return $ LetStmt ident expr

