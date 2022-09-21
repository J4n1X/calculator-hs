{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module CalcParser where

import Debug.Trace
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


braces :: Parser a -> Parser a
braces a = between (reservedOp "{") (reservedOp "}") a

brackets :: Parser a -> Parser a
brackets a = between (reservedOp "[") (reservedOp "]") a

expr :: Parser Expr
expr = Ex.buildExpressionParser opTable factor

typ :: Parser CalcType
typ = try intType
  <|> try byteType
  <|> try floatType
  <|> try arrayType
  <?> "type"
  where
    intType = do
      reserved "int"
      return CalcInteger
    byteType = do
      reserved "byte"
      return CalcByte
    floatType = do
      reserved "float"
      return CalcFloat
    arrayType = do
      reserved "array"
      (t, s) <- brackets arrDef
      return $ CalcArray t s
    arrDef = do
      t <- typ
      reservedOp ";"
      s <- integer
      return (t, s)

floating :: Parser Expr
floating = Value . FloatValue <$> float

-- For now, we only support doubles
int :: Parser Expr
int = Value . IntegerValue . fromInteger <$> integer

byte :: Parser Expr
byte = Value . ByteValue . fromInteger <$> integer

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
       <|> try byte
       <|> try call
       <|> variableRef
       <|> parens expr
       <?> "factor"

variableDef :: Parser Stmt
variableDef = do
  name <- identifier
  reservedOp ":"
  t <- typ
  return $ VarDecl name (Just t) Nothing

varDecl :: Parser Stmt
varDecl = do
  name <- identifier
  t <- optionMaybe $  reservedOp ":" *> typ
  value <- reservedOp "=" *> expr
  return $ VarDecl name t (Just value)

function :: Parser Stmt
function = do
    --reserved "fun"
  name <- identifier
  args <- parens $ commaSep variableDef
  t <- optionMaybe $ reservedOp ":" *> typ
  reservedOp "="
  Function name t args <$> stmt

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


stmt :: Parser Stmt
stmt = try varDecl
   <|> try stmtExpr
   <|> try ifCondStmt
   <|> try stmtBlock
   <?> "statement"

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
       <?> "top level statement"

topLevel :: Parser [Stmt]
topLevel = many topStmts
--  Tok.whiteSpace lexer

parseExpr :: String -> String -> Either ParseError Expr
parseExpr name s = parse (contents expr) name s

parseStmt :: SourceName -> String -> Either ParseError Stmt
parseStmt name s = parse (contents stmt) name s

parseTopLevel :: SourceName -> String -> Either ParseError [Stmt]
parseTopLevel name s = parse (contents topLevel) name s