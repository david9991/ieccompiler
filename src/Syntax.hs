
module Syntax where

import           Text.Parsec

type Name = String

type Type = String

data Expr =
    Float Double
  | Int Integer Integer
  | Ref String
  | RefL String
  | Bool Integer
  | BinOp Op Expr Expr
  | Var String [String] SourcePos
  | Call Name [Expr]
  | FunctionBlock Name [Expr] [Expr]
  | Import Name
  | Program Name Expr [Expr]
  | Block [Expr]
  | VarDeclare [Expr]
  | VarInputDeclare [Expr]
  | VarOutputDeclare [Expr]
  | VarGlobalDeclare [Expr]
  | VarElem Name Type Expr Expr
  | While Expr [Expr]
  | IfBranch Expr [Expr] Expr
  | ElsIfBranch Expr [Expr] Expr
  | ElseBranch [Expr]
  | CaseOf Expr [Expr] Expr
  | CaseCond Expr Expr
  | Repeat Expr [Expr]
  | Configuration Name [Expr]
  | AtMap Name
  | Non
  deriving (Eq, Ord, Show)

data Op = Plus
        | Minus
        | Times
        | Divide
        | Assign
        | OpLT
        | OpGT
        | OpEQ
        | OpNE
        | OpLE
        | OpGE
        | OpAND
        | OpOR
  deriving (Eq, Ord, Show)
