module Lexer where

import           Text.Parsec.Language           ( emptyDef )
import qualified Text.Parsec.Token             as Tok

lexer = Tok.makeTokenParser style
 where
  ops =
    [ "+"
    , "*"
    , "-"
    , "/"
    , ":="
    , "<"
    , "<="
    , "=>"
    , "<>"
    , "="
    , "\""
    , "(*"
    , "*)"
    , "."
    , ":"
    , ";"
    ]

  names =
    [ "PROGRAM"
    , "END_PROGRAM"
    , "VAR"
    , "END_VAR"
    , "IMPORT"
    , "END_WHILE"
    , "WHILE"
    , "DO"
    , "FUNCTION_BLOCK"
    , "END_FUNCTION_BLOCK"
    , "VAR_INPUT"
    , "IF"
    , "THEN"
    , "ELSE"
    , "END_IF"
    , "REPEAT"
    , "UNTIL"
    , "END_REPEAT"
    , "ELSIF"
    , "CASE"
    , "END_CASE"
    , "OF"
    , "CONFIGURATION"
    , "VAR_GLOBAL"
    , "AT"
    , "TRUE"
    , "FALSE"
    , "END_CONFIGURATION"
    , "VAR_OUTPUT"
    ]

  style = emptyDef { Tok.commentLine     = "//"
                   , Tok.commentStart    = "/*"
                   , Tok.commentEnd      = "*/"
                   , Tok.reservedOpNames = ops
                   , Tok.reservedNames   = names
                   }

integer = Tok.integer lexer

float = Tok.float lexer

parens = Tok.parens lexer

commaSep = Tok.commaSep lexer

semiSep = Tok.semiSep lexer

semi = Tok.semi lexer

identifier = Tok.identifier lexer

reserved = Tok.reserved lexer

reservedOp = Tok.reservedOp lexer
