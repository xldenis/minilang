{-# LANGUAGE OverloadedStrings #-}

module Minilang.Parser (minilang) where

import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Expr
import qualified Data.ByteString.Char8 as B
import Control.Applicative
import Control.Monad
import System.Environment

import Minilang.Types

table = [ [unary "-" Neg]
        , [binary "*" Mult, binary "/" Div]
        , [binary "+" Plus, binary "-" Minus]
        ]

binary op tCons = Infix (do{trim op; return $ AOp tCons}) AssocLeft 
unary op tCons = Prefix (do{trim op; return $ tCons})

reserved = ["if", ":", "while", "do", "endif", "done", "else", "var", "print", "read", "then"]

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = do { open; x <- p; close; return x }

symbol :: B.ByteString -> Parser B.ByteString
symbol s = do { r <- string s; spaces; return r }

parens :: Parser a -> Parser a
parens = between "(" ")"

spaces :: Parser ()
spaces = skipSpace

spaces1 :: Parser ()
spaces1 = space >> skipSpace

trim :: Parser a -> Parser a
trim = between spaces spaces

identifier :: Parser String
identifier = try $ do
  name <- B.cons <$> satisfy (inClass "_a-zA-Z") <*> takeTill (notInClass "_a-zA-Z0-9")
  if (elem name reserved)
  then fail $ "cannot use reserved word " ++  B.unpack name ++" as identifier" 
  else return $ B.unpack name

simpleExpr :: Parser a -> Parser t -> (t->b) -> Parser b
simpleExpr key body cons = do
  key <* spaces1
  var <- body
  ";"
  return $ cons var 

varExpr :: Parser a -> Parser b -> Parser c -> (a->c->d) -> Parser d
varExpr l m r cons = do
  left <- l
  trim m
  right <- r
  ";"
  return $ cons left right

expr :: Parser Expr
expr = spaces *> (comment <|> ifExpr <|> whileExpr <|> readExpr <|> printExpr <|> assign)


ifExpr :: Parser Expr
ifExpr = do
  "if" <* spaces1
  cond <- aExpr <* spaces1
  "then" <* spaces1
  pass <- (many' expr)
  fail <- option [] (spaces *> "else" *> space *> (many' expr))
  spaces1 *> "endif"
  return $ If cond pass fail

whileExpr :: Parser Expr
whileExpr = do
  "while" <* spaces1
  cond <- aExpr <* spaces1
  "do" <* spaces1
  body <- (many' expr) <* spaces1
  "done"
  return $ While cond body

readExpr :: Parser Expr
readExpr = (simpleExpr "read" identifier Read) <|> (fail "Invalid Read")

printExpr :: Parser Expr
printExpr = simpleExpr "print" aExpr Print <|> (fail "Invalid Print")

decl :: Parser Expr
decl = varExpr ("var" *> spaces1 *> identifier ) ":" (B.unpack <$> ("int" <|> "float")) Decl <|> (fail "Invlaid Declaration")

assign :: Parser Expr
assign = varExpr identifier "=" aExpr Assign <|> (fail "Invalid Assign")

comment :: Parser Expr
comment = "#" *> takeTill (\x -> x=='\n' || x== '\r') *> return Nop <|> (fail "Invalid Comment")

aExpr :: Parser ArithExpr
aExpr = buildExpressionParser table aTerm <|> (fail "Invalid Arithmetic")

aCons :: Parser ArithExpr
aCons = do
  h <- (B.unpack <$> "0") <|> (:) <$> satisfy (inClass "1-9") <*> many' digit
  return $ ICons (read (h))

fCons :: Parser ArithExpr
fCons = do
  int <- (B.unpack <$> "0") <|> (:) <$> satisfy (inClass "1-9") <*> many' digit
  "."
  frac <- many1 digit
  return $ FCons (read (int ++['.']++frac))

aVar :: Parser ArithExpr
aVar = do
  var <- identifier
  return $ ARef var

aTerm :: Parser ArithExpr
aTerm = ((parens (trim aExpr)) <|> fCons <|> aCons <|> aVar)

minilang :: Parser MiniProg
minilang = do
  decls <- many' $ spaces *> (decl <|> comment)
  exprs <- manyTill' expr (spaces *> endOfInput)
  return $ decls ++ exprs
