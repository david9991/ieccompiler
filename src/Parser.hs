

module Parser where

import           Codegen
import           Data.Tree.Pretty
import           Lexer
import           Syntax
import           Text.Parsec
import qualified Text.Parsec.Expr              as Ex
import           Text.Parsec.String             ( Parser )
import qualified Text.Parsec.Token             as Tok
import           Tree

binary s f = Ex.Infix (reservedOp s >> return (BinOp f))

logic s f = Ex.Infix (reservedOp s >> return (BinOp f))

table =
  [ [binary "*" Times Ex.AssocLeft, binary "/" Divide Ex.AssocLeft]
  , [binary "+" Plus Ex.AssocLeft, binary "-" Minus Ex.AssocLeft]
  , [ binary "<"  OpLT Ex.AssocLeft
    , binary ">"  OpGT Ex.AssocLeft
    , binary "<=" OpLE Ex.AssocLeft
    , binary ">=" OpGE Ex.AssocLeft
    , binary "<>" OpNE Ex.AssocLeft
    , binary "="  OpEQ Ex.AssocLeft
    ]
  , [logic "AND" OpAND Ex.AssocLeft, logic "OR" OpOR Ex.AssocLeft]
  , [binary ":=" Assign Ex.AssocLeft]
  ]

skipWhitespaces = many $ oneOf "\t \r\n"

-- TODO 32bit Integer
int :: Parser Expr
int = Int 16 <$> integer

floating :: Parser Expr
floating = do
  f <- float
  p <- getPosition
  if floor f /= ceiling f
    then error ("Not a float " ++ show p)
    else return $ Float f

bool :: Parser Expr
bool = try boolTrue <|> try boolFalse

boolTrue :: Parser Expr
boolTrue = do
  reserved "TRUE"
  return $ Bool 1

boolFalse :: Parser Expr
boolFalse = do
  reserved "FALSE"
  return $ Bool 0

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

block :: Parser [Expr]
block = expr `sepEndBy1` semi

comment2 :: Parser Expr
comment2 = do
  reserved "(*"
  many $ noneOf "*)"
  reserved "*)"
  return Non

variable :: Parser Expr
variable = do
  var <- identifier
  ext <- many $ do
    reservedOp "."
    identifier
  Var var ext <$> getPosition

repeatuntil :: Parser Expr
repeatuntil = do
  reserved "REPEAT"
  body <- many chunk
  reserved "UNTIL"
  cond <- expr
  reservedOp ";"
  reserved "END_REPEAT"
  return $ Repeat cond body

functionBlock :: Parser Expr
functionBlock = do
  reserved "FUNCTION_BLOCK"
  name   <- identifier
  vardef <- many $ try inputdefs <|> try outputdefs <|> try vardefs
  body   <- many chunk
  reserved "END_FUNCTION_BLOCK"
  return $ FunctionBlock name vardef body

extern :: Parser Expr
extern = do
  reserved "IMPORT"
  char '\"'
  name <- many $ noneOf "\""
  char '\"'
  skipWhitespaces
  return $ Import name

initializer :: Parser Expr
initializer = do
  reservedOp ":="
  expr

whileloop :: Parser Expr
whileloop = do
  reserved "WHILE"
  c <- expr
  reserved "DO"
  b <- many chunk
  reserved "END_WHILE"
  return $ While c b

configuration :: Parser Expr
configuration = do
  reserved "CONFIGURATION"
  n <- identifier
  v <- many $ try globaldefs
  reserved "END_CONFIGURATION"
  return $ Configuration n v

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

atmap :: Parser Expr
atmap = do
  reserved "AT"
  at <- many $ noneOf ":"
  return $ AtMap at

varelem :: Parser Expr
varelem = do
  name <- identifier
  at   <- try atmap <|> return Non
  reservedOp ":"
  type_ <- identifier
  val   <- try initializer <|> return Non
  return $ VarElem name type_ val at

vardefs :: Parser Expr
vardefs = do
  reserved "VAR"
  vars <- varelem `sepEndBy1` semi
  reserved "END_VAR"
  return $ VarDeclare vars

globaldefs :: Parser Expr
globaldefs = do
  reserved "VAR_GLOBAL"
  vars <- varelem `sepEndBy1` semi
  reserved "END_VAR"
  return $ VarGlobalDeclare vars

inputdefs :: Parser Expr
inputdefs = do
  reserved "VAR_INPUT"
  vars <- varelem `sepEndBy1` semi
  reserved "END_VAR"
  return $ VarInputDeclare vars

outputdefs :: Parser Expr
outputdefs = do
  reserved "VAR_OUTPUT"
  vars <- varelem `sepEndBy1` semi
  reserved "END_VAR"
  return $ VarOutputDeclare vars

elsifbranch :: Parser Expr
elsifbranch = do
  reserved "ELSIF"
  cond <- expr
  reserved "THEN"
  body <- many chunk
  el   <- try elsebranch <|> try elsifbranch <|> return Non
  return $ ElsIfBranch cond body el

elsebranch :: Parser Expr
elsebranch = do
  reserved "ELSE"
  body <- many chunk
  return $ ElseBranch body

ifbranch :: Parser Expr
ifbranch = do
  reserved "IF"
  cond <- expr
  reserved "THEN"
  body <- many chunk
  el   <- try elsebranch <|> try elsifbranch <|> return Non
  reserved "END_IF"
  return $ IfBranch cond body el

caseof :: Parser Expr
caseof = do
  reserved "CASE"
  var <- expr
  reserved "OF"
  case_ <- many $ do
    cond <- expr
    reservedOp ":"
    body <- expr
    reservedOp ";"
    return $ CaseCond cond body
  el <- elsebranch
  reserved "END_CASE"
  return $ CaseOf var case_ el

chunk :: Parser Expr
chunk = Block <$> block

program :: Parser Expr
program = do
  reserved "PROGRAM"
  name   <- identifier
  vardef <- try vardefs <|> return Non
  body   <- many chunk
  reserved "END_PROGRAM"
  return $ Program name vardef body

factor :: Parser Expr
factor =
  try floating
    <|> try int
    <|> try bool
    <|> try call
    <|> functionBlock
    <|> ifbranch
    <|> caseof
    <|> repeatuntil
    <|> whileloop
    <|> variable
    <|> parens expr

defn :: Parser Expr
defn =
  try extern
    <|> try functionBlock
    <|> try program
    <|> try comment2
    <|> try configuration

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many defn

parseToplevel :: String -> String -> Either ParseError [Expr]
parseToplevel = parse (contents toplevel)

processAst :: String -> String -> IO String
processAst file cont = do
  let res = parseToplevel file cont
  case res of
    Left  err -> fail (show err)
    Right ex  -> return $ drawVerticalTree $ astTree ex

processIR :: String -> String -> IO String
processIR file cont = do
  let res = parseToplevel file cont
  case res of
    Left  err -> fail (show err)
    Right ex  -> return $ outputLL ex
