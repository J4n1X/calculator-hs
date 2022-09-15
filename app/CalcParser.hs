{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module CalcParser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import CalcLexer
import CalcDefs
import Text.ParserCombinators.ReadP (skipSpaces)

binary s f assoc = Ex.Infix (reservedOp s >> return (BinOp f)) assoc

opTable = [[binary "*"  Times        Ex.AssocLeft,
            binary "/"  Divide       Ex.AssocLeft],
           [binary "+"  Plus         Ex.AssocLeft,
            binary "-"  Minus        Ex.AssocLeft],
           [binary "==" Equal        Ex.AssocLeft,
            binary "!=" NotEqual     Ex.AssocLeft,
            binary ">"  Greater      Ex.AssocLeft,
            binary ">=" GreaterEqual Ex.AssocLeft,
            binary "<"  Less         Ex.AssocLeft,
            binary "<=" LessEqual    Ex.AssocLeft]]


expr :: Parser Expr
expr = Ex.buildExpressionParser opTable factor

floating :: Parser Expr
floating = Float <$> float

-- For now, we only support doubles
int :: Parser Expr
int = Float . fromInteger <$> integer

braces :: Parser a -> Parser a
braces a = between (reserved "{") (reserved "}") a

call :: Parser Expr
call = do
  name <- identifier
  exprs <- parens $ commaSep expr
  return $ Call name exprs

variableRef :: Parser Expr
variableRef = Variable <$> identifier

factor :: Parser Expr
factor = try floating
       <|> try int
       <|> try call
       <|> variableRef
       <|> parens expr

variableDef :: Parser Stmt
variableDef = do
  name <- identifier
  return $ VarDecl name Nothing

function :: Parser Stmt
function = do
  -- reserved "fun"
  name <- identifier
  args <- parens $ commaSep variableDef
  reserved "="
  Function name args <$> stmt

ifCondStmt :: Parser Stmt
ifCondStmt = do
  reserved "if"
  cond <- parens expr
  trueBody <- stmt
  IfCond cond trueBody <$> optionMaybe elseBody
  where
    elseBody = do
      reserved "else" 
      stmt


stmtBlock :: Parser Stmt
stmtBlock = Block <$> braces (semiSep stmt)

stmtExpr :: Parser Stmt
stmtExpr = StmtExpr <$> expr

varDecl :: Parser Stmt
varDecl = do
  name <- identifier
  reserved "="
  VarDecl name . Just <$> expr


stmt :: Parser Stmt
stmt = try varDecl
   <|> try stmtExpr
   <|> try ifCondStmt
   <|> try stmtBlock

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

topStmts :: Parser Stmt
topStmts = try function
       <|> try varDecl
       <|> try stmtExpr

topLevel :: Parser [Stmt]
topLevel = many topStmts
--  Tok.whiteSpace lexer

parseExpr :: String -> String -> Either ParseError Expr
parseExpr name s = parse (contents expr) name s

parseStmt :: SourceName -> String -> Either ParseError Stmt
parseStmt name s = parse (contents stmt) name s

parseTopLevel :: SourceName -> String -> Either ParseError [Stmt]
parseTopLevel name s = parse (contents topLevel) name s