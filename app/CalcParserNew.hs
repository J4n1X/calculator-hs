{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module CalcParserNew where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import CalcLexer
import CalcDefs

binary s f assoc = Ex.Infix (reservedOp s >> return (BinOp f)) assoc

opTable = [[binary "*" Times Ex.AssocLeft,
          binary "/"   Divide Ex.AssocLeft]
        ,[binary "+"   Plus Ex.AssocLeft,
          binary "-"   Minus Ex.AssocLeft]]


expr :: Parser Expr
expr = Ex.buildExpressionParser opTable factor

floating :: Parser Expr
floating = Float <$> float

-- For now, we only support doubles
int :: Parser Expr
int = Float . fromInteger <$> integer 

factor :: Parser Expr
factor = try floating
       <|> try int
       <|> parens expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r