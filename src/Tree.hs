module Tree where

import           Data.Tree
import           Syntax

astTree :: [Expr] -> Tree String
astTree x = Node "AST" $ subTree x

getName :: Expr -> String
getName (Program n _ _     )  = n
getName (VarElem n t _ _   )  = n ++ ":" ++ t
getName (VarDeclare       _)  = "LocalVar"
getName (VarInputDeclare  _)  = "Args"
getName (VarGlobalDeclare _)  = "GlobalVar"
getName (VarOutputDeclare _)  = "OutputVar"
getName (BinOp op _  _     )  = show op
getName (Var   n  [] _     )  = n
getName (Var   n  e  _     )  = n ++ "." ++ show e
getName (Float f           )  = show f
getName (Call  n _         )  = "Call " ++ n ++ "()"
getName (While _ _         )  = "While Loop"
getName (Int   _ v         )  = show v
getName Non                   = "Non"
getName (Import i)            = "Import:" ++ i
getName (Block  _)            = "Block"
getName IfBranch{}            = "If"
getName (ElseBranch _)        = "Else"
getName ElsIfBranch{}         = "Elsif"
getName CaseOf{}              = "Case"
getName (CaseCond _ _       ) = "CaseCond"
getName (FunctionBlock n _ _) = "FB:" ++ n
getName (Repeat        _ _  ) = "Repeat"
getName (Configuration n _  ) = "Cfg:" ++ n
getName (AtMap n            ) = "At:" ++ n
getName (Bool  n            ) = show n
getName (Ref   _            ) = "Ref"
getName (RefL  _            ) = "RefL"

getSub :: Expr -> [Expr]
getSub (Program _ v b      ) = v : b
getSub (Block            b ) = b
getSub (VarDeclare       v ) = v
getSub (VarInputDeclare  v ) = v
getSub (VarOutputDeclare v ) = v
getSub (VarGlobalDeclare v ) = v
getSub (BinOp _ a b        ) = [a, b]
getSub (VarElem _ _ i a    ) = i : [a]
getSub (While c b          ) = c : b
getSub (Call  _ a          ) = a
getSub (IfBranch c t e     ) = [c] ++ t ++ [e]
getSub (ElseBranch t       ) = t
getSub (ElsIfBranch c t e  ) = [c] ++ t ++ [e]
getSub (CaseOf      c t e  ) = [c] ++ t ++ [e]
getSub (CaseCond c t       ) = c : [t]
getSub (FunctionBlock _ v t) = v ++ t
getSub (Repeat        c t  ) = c : t
getSub (Configuration _ v  ) = v
getSub _                     = []

subTree :: [Expr] -> Forest String
subTree (x : xs) = Node name (subTree sub) : subTree xs
  where (name, sub) = (getName x, getSub x)
subTree [] = []
