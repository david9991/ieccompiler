{-# LANGUAGE OverloadedStrings #-}

module Codegen where

import           Control.Monad.State
import           Data.Map                      as Map
import           Data.Strings
import           Data.Text.Lazy                as L
import           Data.Word
import           LLVM.AST                      as AST
import qualified LLVM.AST.AddrSpace            as A
import qualified LLVM.AST.CallingConvention    as CC
import qualified LLVM.AST.Constant             as C
import qualified LLVM.AST.Float                as F
import qualified LLVM.AST.FloatingPointPredicate
                                               as FP
import           LLVM.AST.Global               as G
import qualified LLVM.AST.IntegerPredicate     as I
import           LLVM.AST.ParameterAttribute
import qualified LLVM.AST.Type                 as T
import           LLVM.Pretty
import qualified Syntax                        as S

module_ :: [S.Expr] -> Module
module_ e = evalState (buildLLVM e) emptyLLVM

emptyLLVM :: LLVM_
emptyLLVM = LLVM_ { astModule    = defaultModule { moduleName = "Module" }
                  , globalsymtbl = Map.empty
                  , globaltypes  = Map.empty
                  , globalobjs   = Map.empty
                  , globalmap    = Map.empty
                  }

buildLLVM :: [S.Expr] -> LLVM Module
buildLLVM x = do
  Prelude.foldr buildDef (return ()) x
  buildDataUpdater
  gets astModule

buildDataUpdater :: LLVM ()
buildDataUpdater = do
  gmap <- gets globalmap
  gs   <- gets globalsymtbl
  gt   <- gets globaltypes
  go   <- gets globalobjs
  addDefs $ evalState (buildAllMapper (toList gmap)) (emptyFunction gs gt go)

buildAllMapper
  :: [(Name, (Name, (Maybe Integer, Maybe Integer)))]
  -> FunctionState [Definition]
buildAllMapper a = do
  addBlock
  buildMapper a
  cb  <- gets currentBlock
  cbs <- getBlock cb
  modifyBlock cb cbs { term = Just $ Do $ Ret Nothing [] }
  bs   <- gets blocks
  blks <- Prelude.foldr iterBlock (return []) bs
  return
    [ GlobalDefinition functionDefaults { G.name = mkName "__refresh_global"
                                        , parameters  = ([], False)
                                        , returnType  = T.void
                                        , basicBlocks = blks
                                        }
    ]

buildMapper
  :: [(Name, (Name, (Maybe Integer, Maybe Integer)))] -> FunctionState ()
buildMapper ((n, (v, (o, b))) : xs) = do
  gsym <- gets gsymtbl
  case Map.lookup n gsym of
    Just (var__, _) -> do
      var_ <- emitInst ITLocalOp
                       (getOperandType var__)
                       Nothing
                       (Load False (getOperandPtr var__) Nothing 4 [])
      let Just (_, var) = var_
      case Map.lookup v gsym of
        Just (dir, dirname) -> do
          let Just off = o
          src_ <- emitInst
            ITLocalOp
            T.i32
            Nothing
            (GetElementPtr
              True
              (getOperandPtr dir)
              [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 off)]
              []
            )
          let Just (_, src) = src_
          mem_ <- emitInst ITLocalOp
                           (getOperandType src)
                           Nothing
                           (Load False (getOperandPtr src) Nothing 4 [])
          let Just (_, mem) = mem_
          case dirname of
            "IX" -> do
              (l, r) <- promoteType True var__ mem
              case b of
                Just _ -> do
                  (clr, recov) <- defaults r
                  (l, r) <- promoteType True l (ConstantOperand $ C.Int 32 1)
                  emitInst ITNoOp
                           (getOperandType l)
                           Nothing
                           (Store False (getOperandPtr l) r Nothing 4 [])
                  modify $ \s -> s { currentBlock = clr }
                  (l, r) <- promoteType True l (ConstantOperand $ C.Int 32 0)
                  emitInst ITNoOp
                           (getOperandType l)
                           Nothing
                           (Store False (getOperandPtr l) r Nothing 4 [])
                  modify $ \s -> s { currentBlock = recov }
                Nothing -> do
                  emitInst ITNoOp
                           (getOperandType l)
                           Nothing
                           (Store False (getOperandPtr l) r Nothing 4 [])
                  return ()
            "QX" -> do
              (l, r) <- promoteType True src var
              case b of
                Just bit -> do
                  (clr, recov) <- defaults r
                  sh_          <- emitInst
                    ITLocalOp
                    (getOperandType l)
                    Nothing
                    (Shl False
                         False
                         (ConstantOperand $ C.Int 32 1)
                         (ConstantOperand $ C.Int 32 bit)
                         []
                    )
                  let Just (_, sh) = sh_
                  or_ <- emitInst ITLocalOp
                                  (getOperandType l)
                                  Nothing
                                  (Or mem sh [])
                  let Just (_, or) = or_
                  (l, r) <- promoteType True l or
                  emitInst ITNoOp
                           (getOperandType l)
                           Nothing
                           (Store False (getOperandPtr l) r Nothing 4 [])
                  modify $ \s -> s { currentBlock = clr }
                  sh_ <- emitInst
                    ITLocalOp
                    (getOperandType l)
                    Nothing
                    (Shl False
                         False
                         (ConstantOperand $ C.Int 32 1)
                         (ConstantOperand $ C.Int 32 bit)
                         []
                    )
                  let Just (_, sh) = sh_
                  emitInst ITLocalOp
                           (getOperandType l)
                           Nothing
                           (Xor (ConstantOperand $ C.Int 32 0xffffffff) sh [])
                  and_ <- emitInst ITLocalOp
                                   (getOperandType l)
                                   Nothing
                                   (And mem sh [])
                  let Just (_, and) = and_
                  (l, r) <- promoteType True l and
                  emitInst ITNoOp
                           (getOperandType l)
                           Nothing
                           (Store False (getOperandPtr l) r Nothing 4 [])
                  modify $ \s -> s { currentBlock = recov }
                Nothing -> do
                  emitInst ITNoOp
                           (getOperandType l)
                           Nothing
                           (Store False (getOperandPtr l) r Nothing 4 [])
                  return ()
            _ -> error "Internal error"
          buildMapper xs
        _ -> error "Internal error"
    Nothing -> error "Internal error"
 where
  condi r = do
    cond_ <- emitInst ITLocalOp
                      T.i1
                      Nothing
                      (ICmp I.NE r (ConstantOperand $ C.Int 32 0) [])
    let Just (_, cond) = cond_
    return cond

  defaults r = do
    cond1 <- condi r
    st1   <- gets currentBlock
    st1s  <- getBlock st1
    recov <- addBlock
    set   <- addBlock
    sets  <- getBlock set
    clr   <- addBlock
    clrs  <- getBlock clr
    modifyBlock st1 st1s { term = Just $ Do $ CondBr cond1 set clr [] }
    modifyBlock set sets { term = Just $ Do $ Br recov [] }
    modifyBlock clr clrs { term = Just $ Do $ Br recov [] }
    modify $ \s -> s { currentBlock = set }
    return (clr, recov)
buildMapper [] = return ()

type LLVM a = State LLVM_ a

type FunctionState a = State FunctionState_ a

type BlockState a = State BlockState_ a

data LLVM_ = LLVM_
  { astModule    :: AST.Module
  , globalsymtbl :: Map Name (Operand, String)
  , globaltypes  :: Map Name Type
  , globalobjs   :: Map Name [(Name, Type)]
  , globalmap    :: Map Name (Name, (Maybe Integer, Maybe Integer))
  }

data FunctionState_ = FunctionState_
  { currentBlock :: Name
  , symtbl       :: Map Name (Operand, String)
  , gsymtbl      :: Map Name (Operand, String)
  , osymtbl      :: [(Name, Type)]
  , gtypes       :: Map Name Type
  , inputs       :: [(Name, Type)]
  , blocks       :: Map Name BlockState_
  , blockCount   :: Int
  , varIdx       :: Int
  , gotypes      :: Map Name [(Name, Type)]
  }

data BlockState_ = BlockState_
  { name  :: Name
  , stack :: [Named Instruction]
  , term  :: Maybe (Named Terminator)
  }

data InstType = ITNoOp
              | ITLocalOp

addDefs :: [Definition] -> LLVM ()
addDefs d = do
  mod <- gets astModule
  let old = moduleDefinitions mod
  modify $ \s -> s { astModule = mod { moduleDefinitions = old ++ d } }

buildDef :: S.Expr -> LLVM () -> LLVM ()
buildDef (S.Program n v b) y = do
  gs <- gets globalsymtbl
  gt <- gets globaltypes
  go <- gets globalobjs
  addDefs $ evalState (buildProgram n v b) (emptyFunction gs gt go)
  y
buildDef (S.Configuration _ v) y = do
  buildGlobalVars v
  y
buildDef (S.FunctionBlock n v b) y = do
  gs <- gets globalsymtbl
  gt <- gets globaltypes
  go <- gets globalobjs
  let (a, s) = runState (buildFunctionBlock n v b) (emptyFunction gs gt go)
  addDefs a
  go <- gets globalobjs
  modify $ \s2 -> s2 { globalobjs = insert (mkName n) (osymtbl s) go }
  let sym   = osymtbl s
  let type_ = buildStructure sym
  let input = inputs s
  modify $ \s -> s { globaltypes = insert (mkName n) type_ gt }
  modify $ \s -> s
    { globalsymtbl = insert
                       (mkName n)
                       ( ConstantOperand $ C.GlobalReference
                         (FunctionType VoidType (extractTypes input) False)
                         (mkName n)
                       , n
                       )
                       gs
    }
  y
buildDef (S.Import _) y = y -- TODO Implement
buildDef S.Non        y = y
buildDef _            _ = error "Internal error"

extractTypes :: [(Name, Type)] -> [Type]
extractTypes = Prelude.foldr (\x -> (++) [snd x]) []

buildFunctionBlock
  :: String -> [S.Expr] -> [S.Expr] -> FunctionState [Definition]
buildFunctionBlock n v b = do
  addBlock
  buildEntry S.Non
  Prelude.foldr buildFBDef (return ()) v
  Prelude.foldr (buildBlock Nothing) (return ()) b
  bs      <- gets blocks
  blks    <- Prelude.foldr iterBlock (return []) bs
  bc      <- gets blockCount
  osymtbl <- gets osymtbl
  inputs  <- gets inputs
  let objType = buildStructure osymtbl
  return
    [ GlobalDefinition functionDefaults
        { G.name      = mkName n
        , parameters  = ( Parameter (T.PointerType objType (A.AddrSpace 0))
                                    (mkName "_self")
                                    []
                          : buildArguments inputs
                        , False
                        )
        , returnType  = T.void
        , basicBlocks = blks
                          ++ [ BasicBlock (genBlockName Nothing bc)
                                          []
                                          (Do $ Ret Nothing [])
                             ]
        }
    ]

buildStructure :: [(Name, Type)] -> Type
buildStructure t =
  StructureType False (Prelude.foldr buildStructureElement [] t)

buildStructureElement :: (Name, Type) -> [Type] -> [Type]
buildStructureElement t y = snd t : y

buildArguments :: [(Name, Type)] -> [Parameter]
buildArguments = Prelude.foldr buildArgumentElement []

buildArgumentElement :: (Name, Type) -> [Parameter] -> [Parameter]
buildArgumentElement x y = Parameter (snd x) (fst x) [] : y

buildFBDef :: S.Expr -> FunctionState () -> FunctionState ()
buildFBDef (S.VarDeclare x) y = do
  Prelude.foldr buildObjMembers (return ()) x
  y
buildFBDef (S.VarInputDeclare x) y = do
  Prelude.foldr buildFunctionArgs (return ()) x
  y
buildFBDef (S.VarOutputDeclare x) y = do
  Prelude.foldr buildObjMembers (return ()) x
  y
buildFBDef _ _ = error "Internal error"

buildObjMembers :: S.Expr -> FunctionState () -> FunctionState ()
buildObjMembers x y = do
  prevsymtbl <- gets osymtbl
  let (S.VarElem n _ _ _) = x
  (type_, _) <- buildType x
  modify $ \s -> s { osymtbl = prevsymtbl ++ [(mkName n, type_)] }
  y

buildFunctionArgs :: S.Expr -> FunctionState () -> FunctionState ()
buildFunctionArgs x y = do
  previnputs <- gets inputs
  let (S.VarElem n _ _ _) = x
  (type_, _) <- buildType x
  modify $ \s -> s { inputs = previnputs ++ [(mkName n, type_)] }
  y

emptyFunction
  :: Map Name (Operand, String)
  -> Map Name Type
  -> Map Name [(Name, Type)]
  -> FunctionState_
emptyFunction gs gt go = FunctionState_ { Codegen.currentBlock = ""
                                        , symtbl               = Map.empty
                                        , blocks               = Map.empty
                                        , gsymtbl              = gs
                                        , osymtbl              = []
                                        , gtypes               = gt
                                        , inputs               = []
                                        , blockCount           = 0
                                        , varIdx               = 0
                                        , gotypes              = go
                                        }

buildProgram :: String -> S.Expr -> [S.Expr] -> FunctionState [Definition]
buildProgram n v b = do
  addBlock
  buildEntry v
  Prelude.foldr (buildBlock Nothing) (return ()) b
  bs   <- gets blocks
  blks <- Prelude.foldr iterBlock (return []) bs
  bc   <- gets blockCount
  return
    [ GlobalDefinition functionDefaults
        { G.name      = mkName n
        , parameters  = ([], False)
        , returnType  = T.void
        , basicBlocks = blks
                          ++ [ BasicBlock (genBlockName Nothing bc)
                                          []
                                          (Do $ Ret Nothing [])
                             ]
        }
    ]

iterBlock
  :: BlockState_ -> FunctionState [BasicBlock] -> FunctionState [BasicBlock]
iterBlock a b = do
  next <- b
  case term a of
    Just x  -> return $ BasicBlock (Codegen.name a) (stack a) x : next
    Nothing -> error "Internal error"

addBlock :: FunctionState Name
addBlock = do
  blockCount <- gets blockCount
  nextBlock  <- predictNextBlock
  let blockName | blockCount == 0 = Name "entry"
                | otherwise       = nextBlock
  modify $ \s -> s { blockCount = blockCount + 1
                   , Codegen.currentBlock = blockName
                   , blocks = insert blockName (emptyBlock blockName) (blocks s)
                   }
  return blockName

buildBlock :: Maybe Name -> S.Expr -> FunctionState () -> FunctionState ()
buildBlock recov (S.Block x) y = do
  currentBlock <- gets currentBlock
  nextBlock    <- predictNextBlock
  Prelude.foldr (buildStatment recov) (return ()) x
  currentBlockState <- getBlock currentBlock
  case term currentBlockState of
    Just _  -> return ()
    Nothing -> case recov of
      Just x -> modifyBlock currentBlock
                            currentBlockState { term = Just $ Do $ Br x [] }
      Nothing -> modifyBlock
        currentBlock
        currentBlockState { term = Just $ Do $ Br nextBlock [] }
  y
buildBlock _ _ _ = error "Internal error"

emptyBlock :: Name -> BlockState_
emptyBlock a = BlockState_ { Codegen.name = a, stack = [], term = Nothing }

genBlockName :: Maybe Name -> Int -> Name
genBlockName name idx = case name of
  Just x  -> x
  Nothing -> mkName ("k." ++ show idx)

buildBuiltinMem :: LLVM ()
buildBuiltinMem = do
  addDefs
    [ GlobalDefinition globalVariableDefaults { G.name = mkName "IX"
                                              , G.type' = T.ArrayType 10 T.i32
                                              , G.alignment = 4
                                              }
    , GlobalDefinition globalVariableDefaults { G.name = mkName "QX"
                                              , G.type' = T.ArrayType 10 T.i32
                                              , G.alignment = 4
                                              }
    ]
  globalSymTable <- gets globalsymtbl
  modify $ \s -> s
    { globalsymtbl = insert
      (mkName "IX")
      ( ConstantOperand $ C.GlobalReference (T.ArrayType 10 T.i32) (mkName "IX")
      , "IX"
      )
      globalSymTable
    }
  globalSymTable <- gets globalsymtbl
  modify $ \s -> s
    { globalsymtbl = insert
      (mkName "QX")
      ( ConstantOperand $ C.GlobalReference (T.ArrayType 10 T.i32) (mkName "QX")
      , "QX"
      )
      globalSymTable
    }

buildGlobalVars :: [S.Expr] -> LLVM ()
buildGlobalVars x = do
  buildBuiltinMem
  Prelude.foldr buildVarElements (return ()) x

buildVarElements :: S.Expr -> LLVM () -> LLVM ()
buildVarElements (S.VarGlobalDeclare x) y = Prelude.foldr addVarElement y x
buildVarElements _                      y = y

addVarElement :: S.Expr -> LLVM () -> LLVM ()
addVarElement x y = do
  (gtype, defa) <- buildTypeG x
  addDefs
    [ GlobalDefinition globalVariableDefaults { G.name        = elemname
                                              , G.type'       = gtype
                                              , G.alignment   = 4
                                              , G.initializer = defa
                                              }
    ]
  globalSymTable <- gets globalsymtbl
  modify $ \s -> s
    { globalsymtbl = insert
                       elemname
                       ( ConstantOperand $ C.GlobalReference gtype elemname
                       , elemtype
                       )
                       globalSymTable
    }
  y
 where
  elemname = getVarElemName x

  elemtype = getVarElemType x

buildEntry :: S.Expr -> FunctionState ()
buildEntry (S.VarDeclare v) = do
  Prelude.foldr buildLocalVariables (return ()) v
  currentBlock      <- gets currentBlock
  currentBlockState <- getBlock currentBlock
  nextBlock         <- predictNextBlock
  modifyBlock currentBlock
              currentBlockState { term = Just $ Do $ Br nextBlock [] }
buildEntry S.Non = do
  currentBlock      <- gets currentBlock
  currentBlockState <- getBlock currentBlock
  nextBlock         <- predictNextBlock
  modifyBlock currentBlock
              currentBlockState { term = Just $ Do $ Br nextBlock [] }
buildEntry _ = error "Internal error"

buildConstant :: S.Expr -> Type -> Maybe C.Constant
buildConstant (S.Int b i) _ = Just $ C.Int (read (show b) :: Word32) i
buildConstant (S.Float f) _ =
  Just $ C.Float (F.Single (read (show f) :: Float))
buildConstant _ t = buildDefaultConstant t

buildDefaultConstant :: Type -> Maybe C.Constant
buildDefaultConstant (IntegerType       n) = Just $ C.Int n 0
buildDefaultConstant (FloatingPointType _) = Just $ C.Float (F.Single 0)
buildDefaultConstant (PointerType (IntegerType x) _) =
  Just $ C.IntToPtr (C.Int x 0) T.i32
buildDefaultConstant (StructureType _ x) =
  Just $ C.Struct Nothing False (buildDefaultConstantElem x)
buildDefaultConstant _ = error "Internal error"

buildDefaultConstantElem :: [Type] -> [C.Constant]
buildDefaultConstantElem (x : xs) = case buildDefaultConstant x of
  Just n  -> n : buildDefaultConstantElem xs
  Nothing -> error "Internal error"
buildDefaultConstantElem [] = []

-- TODO Deal with Structure type.
buildType :: S.Expr -> FunctionState (Type, Maybe C.Constant)
buildType (S.VarElem _ "BOOL"  i _) = return (T.i1, buildConstant i T.i1)
buildType (S.VarElem _ "BYTE"  i _) = return (T.i8, buildConstant i T.i8)
buildType (S.VarElem _ "SINT"  i _) = return (T.i8, buildConstant i T.i8)
buildType (S.VarElem _ "INT"   i _) = return (T.i16, buildConstant i T.i16)
buildType (S.VarElem _ "UINT"  i _) = return (T.i16, buildConstant i T.i16)
buildType (S.VarElem _ "WORD"  i _) = return (T.i16, buildConstant i T.i16)
buildType (S.VarElem _ "DINT"  i _) = return (T.i32, buildConstant i T.i32)
buildType (S.VarElem _ "DWORD" i _) = return (T.i32, buildConstant i T.i32)
buildType (S.VarElem _ "LINT"  i _) = return (T.i64, buildConstant i T.i64)
buildType (S.VarElem _ "LWORD" i _) = return (T.i64, buildConstant i T.i64)
buildType (S.VarElem _ "REAL"  i _) = return (T.float, buildConstant i T.float)
buildType (S.VarElem _ "LREAL" i _) =
  return (T.double, buildConstant i T.double)
buildType (S.VarElem _ ty _ _) = do
  tys <- gets gtypes
  case Map.lookup (mkName ty) tys of
    Just x  -> return (x, buildConstant S.Non x)
    Nothing -> error ("Type " ++ ty ++ " is undefined")
buildType _ = error "Internal error"

buildTypeG :: S.Expr -> LLVM (Type, Maybe C.Constant)
buildTypeG (S.VarElem n "BOOL" i p) = do
  buildMap n p
  return (T.i1, buildConstant i T.i1)
buildTypeG (S.VarElem n "BYTE" i p) = do
  buildMap n p
  return (T.i8, buildConstant i T.i8)
buildTypeG (S.VarElem n "SINT" i p) = do
  buildMap n p
  return (T.i8, buildConstant i T.i8)
buildTypeG (S.VarElem n "INT" i p) = do
  buildMap n p
  return (T.i16, buildConstant i T.i16)
buildTypeG (S.VarElem n "UINT" i p) = do
  buildMap n p
  return (T.i16, buildConstant i T.i16)
buildTypeG (S.VarElem n "WORD" i p) = do
  buildMap n p
  return (T.i16, buildConstant i T.i16)
buildTypeG (S.VarElem n "DINT" i p) = do
  buildMap n p
  return (T.i32, buildConstant i T.i32)
buildTypeG (S.VarElem n "DWORD" i p) = do
  buildMap n p
  return (T.i32, buildConstant i T.i32)
buildTypeG (S.VarElem n "LINT" i p) = do
  buildMap n p
  return (T.i64, buildConstant i T.i64)
buildTypeG (S.VarElem n "LWORD" i p) = do
  buildMap n p
  return (T.i64, buildConstant i T.i64)
buildTypeG (S.VarElem n "REAL" i p) = do
  buildMap n p
  return (T.float, buildConstant i T.float)
buildTypeG (S.VarElem n "LREAL" i p) = do
  buildMap n p
  return (T.double, buildConstant i T.double)
buildTypeG (S.VarElem _ ty _ _) = do
  tys <- gets globaltypes
  case Map.lookup (mkName ty) tys of
    Just x  -> return (x, buildConstant S.Non x)
    Nothing -> error ("Type " ++ ty ++ " is undefined")
buildTypeG _ = error "Internal error"

buildMap :: String -> S.Expr -> LLVM ()
buildMap n (S.AtMap m) = do
  gmap <- gets globalmap
  let (nm, b) = ioMatch m
  modify $ \s -> s { globalmap = insert (mkName n) (nm, b) gmap }
buildMap _ S.Non = return ()
buildMap _ _     = error "Internal error"

ioMatch :: String -> (Name, (Maybe Integer, Maybe Integer))
ioMatch x
  | Prelude.take 3 x == "%IX" = ("IX", ioMatchPos (Prelude.drop 3 (strTrim x)))
  | Prelude.take 3 x == "%QX" = ("QX", ioMatchPos (Prelude.drop 3 (strTrim x)))
  | otherwise                 = error "Internal error"

ioMatchPos :: String -> (Maybe Integer, Maybe Integer)
ioMatchPos p = do
  let part = strSplitAll "." p
  if Prelude.length part > 1
    then
      ( Just (read (Prelude.head part) :: Integer)
      , Just (read (part !! 1) :: Integer)
      )
    else (Just (read (Prelude.head part) :: Integer), Nothing)

getVarElemName :: S.Expr -> Name
getVarElemName (S.VarElem n _ _ _) = mkName n
getVarElemName _                   = error "Internal error"

getVarElemType :: S.Expr -> String
getVarElemType (S.VarElem _ n _ _) = n
getVarElemType _                   = error "Internal error"

buildLocalVariables :: S.Expr -> FunctionState () -> FunctionState ()
buildLocalVariables x y = do
  symTable <- gets symtbl
  (type_, _) <- buildType x
  inst <- emitInst ITLocalOp type_ (Just elemname) (Alloca type_ Nothing 4 [])
  case inst of
    Just x -> modify
      $ \s -> s { symtbl = insert type__ (op__, elemtype) symTable }
      where (type__, op__) = x
    Nothing -> return ()
  y
 where
  elemname = getVarElemName x

  elemtype = getVarElemType x

buildStatment :: Maybe Name -> S.Expr -> FunctionState () -> FunctionState ()
buildStatment r x y = do
  buildInst r x
  y

modifyBlock :: Name -> BlockState_ -> FunctionState ()
modifyBlock cb new = do
  blks <- gets blocks
  modify $ \s -> s { blocks = Map.insert cb new blks }

getBlock :: Name -> FunctionState BlockState_
getBlock n = do
  blks <- gets blocks
  case Map.lookup n blks of
    Just x  -> return x
    Nothing -> error $ show n

procOperand :: Maybe Name -> S.Expr -> Bool -> FunctionState Operand
procOperand rc expr simple = do
  orgOperand <- getOperand simple expr
  extend rc orgOperand expr
 where
  extend r a b = case a of
    Just x  -> return x
    Nothing -> do
      t <- buildInst r b
      case t of
        Just y  -> return y
        Nothing -> error "Internal error"

buildInst :: Maybe Name -> S.Expr -> FunctionState (Maybe Operand)
buildInst rc    (S.BinOp    o l r) = buildBinOp rc o l r
buildInst recov (S.IfBranch c b e) = do
  condExpr          <- buildInst Nothing c
  currentBlock      <- gets currentBlock
  currentBlockState <- getBlock currentBlock
  recover <- if not (Prelude.null b) then Just <$> addBlock else return Nothing
  case recover of
    Just r -> do
      bodyBlock <- addBlock
      Prelude.foldr (buildBlock $ Just r) (return ()) b
      elseBlockT <- buildElseBranch r e
      elseBlock  <- case elseBlockT of
        Just x  -> return x
        Nothing -> return r
      case condExpr of
        Just x -> do
          modifyBlock currentBlock $ currentBlockState
            { term = Just $ Do $ CondBr x bodyBlock elseBlock []
            }
          return Nothing
        Nothing -> error "Internal error"
      modify $ \s -> s { currentBlock = r }
      case recov of
        Just x -> do
          rs <- getBlock r
          modifyBlock r rs { term = Just $ Do $ Br x [] }
          return Nothing
        Nothing -> do
          rs        <- getBlock r
          nextBlock <- predictNextBlock
          modifyBlock r rs { term = Just $ Do $ Br nextBlock [] }
          return Nothing
    Nothing -> return Nothing
buildInst recov (S.Repeat c b) = do
  cb      <- gets currentBlock
  recover <- if not (Prelude.null b) then Just <$> addBlock else return Nothing
  case recover of
    Just r -> do
      body <- addBlock
      Prelude.foldr (buildBlock $ Just r) (return ()) b
      condExpr <- buildInst Nothing c
      case condExpr of
        Just x -> do
          bodyState <- getBlock body
          modifyBlock body bodyState { term = Just $ Do $ CondBr x r body [] }
        Nothing -> error "Internal error"
      modify $ \s -> s { currentBlock = r }
      cbState <- getBlock cb
      case condExpr of
        Just _  -> modifyBlock cb cbState { term = Just $ Do $ Br body [] }
        Nothing -> error "Internal error"
      case recov of
        Just x -> do
          rs <- getBlock r
          modifyBlock r rs { term = Just $ Do $ Br x [] }
          return Nothing
        Nothing -> do
          rs        <- getBlock r
          nextBlock <- predictNextBlock
          modifyBlock r rs { term = Just $ Do $ Br nextBlock [] }
          return Nothing
    Nothing -> return Nothing
buildInst recov (S.While c b) = do
  condExpr <- buildInst Nothing c
  cb       <- gets currentBlock
  recover  <- if not (Prelude.null b) then Just <$> addBlock else return Nothing
  case recover of
    Just r -> do
      body <- addBlock
      Prelude.foldr (buildBlock $ Just r) (return ()) b
      condExpr2 <- buildInst Nothing c
      case condExpr2 of
        Just x -> do
          bodyState <- getBlock body
          modifyBlock body bodyState { term = Just $ Do $ CondBr x body r [] }
        Nothing -> error "Internal error"
      modify $ \s -> s { currentBlock = r }
      cbState <- getBlock cb
      case condExpr of
        Just x ->
          modifyBlock cb cbState { term = Just $ Do $ CondBr x body r [] }
        Nothing -> error "Internal error"
      case recov of
        Just x -> do
          rs <- getBlock r
          modifyBlock r rs { term = Just $ Do $ Br x [] }
          return Nothing
        Nothing -> do
          rs        <- getBlock r
          nextBlock <- predictNextBlock
          modifyBlock r rs { term = Just $ Do $ Br nextBlock [] }
          return Nothing
    Nothing -> return Nothing
buildInst _ (S.Call name args) = do
  ls   <- gets symtbl
  gs   <- gets gsymtbl
  aops <- buildInvokeArguments args
  case Map.lookup (mkName name) ls of
    Just (x, y) -> do
      case Map.lookup (mkName y) gs of
        Just (m, _) -> do
          emitInst
            ITNoOp
            VoidType
            Nothing
            (Call Nothing CC.C [] (Right m) ((getOperandPtr x, []) : aops) [] []
            )
          return Nothing
        Nothing -> error ("Function Block " ++ y ++ " is not found.")
    Nothing -> case Map.lookup (mkName name) gs of
      Just (x, y) -> do
        case Map.lookup (mkName y) gs of
          Just (m, _) -> do
            emitInst
              ITNoOp
              VoidType
              Nothing
              (Call Nothing
                    CC.C
                    []
                    (Right m)
                    ((getOperandPtr x, []) : aops)
                    []
                    []
              )
            return Nothing
          Nothing -> error ("Function Block " ++ y ++ " is not found.")
      Nothing -> error ("Variable " ++ name ++ " is not found.")
buildInst _ e = do
  c      <- procOperand Nothing e False
  (l, r) <- promoteType True c (ConstantOperand $ C.Int 32 0)
  op_    <- emitInst ITLocalOp T.i1 Nothing (ICmp I.NE l r [])
  let Just (_, op) = op_
  return $ Just op

buildInvokeArguments
  :: [S.Expr] -> FunctionState [(Operand, [ParameterAttribute])]
buildInvokeArguments (x : xs) = do
  op   <- procOperand Nothing x False
  next <- buildInvokeArguments xs
  return $ (op, []) : next
buildInvokeArguments [] = return []

predictNextBlock :: FunctionState Name
predictNextBlock = do
  nextBlockNum <- gets blockCount
  return (genBlockName Nothing nextBlockNum)

buildElseBranch :: Name -> S.Expr -> FunctionState (Maybe Name)
buildElseBranch r (S.ElseBranch b) = if not (Prelude.null b)
  then do
    currentBlock      <- addBlock
    currentBlockState <- getBlock currentBlock
    modifyBlock currentBlock $ currentBlockState { term = Just $ Do $ Br r [] }
    Prelude.foldr (buildBlock $ Just r) (return ()) b
    return $ Just currentBlock
  else return Nothing
buildElseBranch r (S.ElsIfBranch c b e) = if not (Prelude.null b)
  then do
    currentBlock      <- addBlock
    currentBlockState <- getBlock currentBlock
    modifyBlock currentBlock $ currentBlockState { term = Just $ Do $ Br r [] }
    Prelude.foldr (buildBlock $ Just r) (return ()) [S.Block [S.IfBranch c b e]]
    return $ Just currentBlock
  else return Nothing
buildElseBranch _ S.Non = return Nothing
buildElseBranch _ _     = error "Internal error"

emitInst
  :: InstType
  -> Type
  -> Maybe Name
  -> Instruction
  -> FunctionState (Maybe (Name, Operand))
emitInst ty t n i = do
  currentBlock      <- gets currentBlock
  currentBlockState <- getBlock currentBlock
  oldIdx            <- gets varIdx
  let name = case n of
        Just x  -> x
        Nothing -> mkName ("l." ++ show oldIdx)
  modify $ \s -> s { varIdx = oldIdx + 1 }
  case ty of
    ITNoOp -> do
      Codegen.modifyBlock
        currentBlock
        (currentBlockState { stack = stack currentBlockState ++ [Do i] })
      return Nothing
    ITLocalOp -> do
      Codegen.modifyBlock
        currentBlock
        (currentBlockState { stack = stack currentBlockState ++ [name := i] })
      return $ Just (name, LocalReference t name)

buildBinOp
  :: Maybe Name -> S.Op -> S.Expr -> S.Expr -> FunctionState (Maybe Operand)
buildBinOp _ S.Plus fl fr = do
  ol     <- procOperand Nothing fl False
  or     <- procOperand Nothing fr False
  (l, r) <- promoteType False ol or
  inst   <- if rankType l > 128
    then emitInst ITLocalOp
                  (getOperandType l)
                  Nothing
                  (FAdd noFastMathFlags l r [])
    else emitInst ITLocalOp (getOperandType l) Nothing (Add False False l r [])
  case inst of
    Just x  -> return $ Just $ snd x
    Nothing -> error "Internal error"
buildBinOp _ S.Minus fl fr = do
  ol     <- procOperand Nothing fl False
  or     <- procOperand Nothing fr False
  (l, r) <- promoteType False ol or
  inst   <- if rankType l > 128
    then emitInst ITLocalOp
                  (getOperandType l)
                  Nothing
                  (FSub noFastMathFlags l r [])
    else emitInst ITLocalOp (getOperandType l) Nothing (Sub False False l r [])
  case inst of
    Just x  -> return $ Just $ snd x
    Nothing -> error "Internal error"
buildBinOp _ S.Times fl fr = do
  ol     <- procOperand Nothing fl False
  or     <- procOperand Nothing fr False
  (l, r) <- promoteType False ol or
  inst   <- if rankType l > 128
    then emitInst ITLocalOp
                  (getOperandType l)
                  Nothing
                  (FMul noFastMathFlags l r [])
    else emitInst ITLocalOp (getOperandType l) Nothing (Mul False False l r [])
  case inst of
    Just x  -> return $ Just $ snd x
    Nothing -> error "Internal error"
buildBinOp _ S.Divide fl fr = do
  ol     <- procOperand Nothing fl False
  or     <- procOperand Nothing fr False
  (l, r) <- promoteType False ol or
  inst   <- if rankType l > 128
    then emitInst ITLocalOp
                  (getOperandType l)
                  Nothing
                  (FDiv noFastMathFlags l r [])
    else emitInst ITLocalOp (getOperandType l) Nothing (SDiv False l r [])
  case inst of
    Just x  -> return $ Just $ snd x
    Nothing -> error "Internal error"
buildBinOp _ S.OpGT fl fr = do
  ol     <- procOperand Nothing fl False
  or     <- procOperand Nothing fr False
  (l, r) <- promoteType False ol or
  inst   <- if rankType l > 128
    then emitInst ITLocalOp T.i1 Nothing (FCmp FP.OGT l r [])
    else emitInst ITLocalOp T.i1 Nothing (ICmp I.SGT l r [])
  case inst of
    Just x  -> return $ Just $ snd x
    Nothing -> error "Internal error"
buildBinOp _ S.OpGE fl fr = do
  ol     <- procOperand Nothing fl False
  or     <- procOperand Nothing fr False
  (l, r) <- promoteType False ol or
  inst   <- if rankType l > 128
    then emitInst ITLocalOp T.i1 Nothing (FCmp FP.OGE l r [])
    else emitInst ITLocalOp T.i1 Nothing (ICmp I.SGE l r [])
  case inst of
    Just x  -> return $ Just $ snd x
    Nothing -> error "Internal error"
buildBinOp _ S.OpEQ fl fr = do
  ol     <- procOperand Nothing fl False
  or     <- procOperand Nothing fr False
  (l, r) <- promoteType False ol or
  inst   <- if rankType l > 128
    then emitInst ITLocalOp T.i1 Nothing (FCmp FP.OEQ l r [])
    else emitInst ITLocalOp T.i1 Nothing (ICmp I.EQ l r [])
  case inst of
    Just x  -> return $ Just $ snd x
    Nothing -> error "Internal error"
buildBinOp _ S.OpNE fl fr = do
  ol     <- procOperand Nothing fl False
  or     <- procOperand Nothing fr False
  (l, r) <- promoteType False ol or
  inst   <- if rankType l > 128
    then emitInst ITLocalOp T.i1 Nothing (FCmp FP.ONE l r [])
    else emitInst ITLocalOp T.i1 Nothing (ICmp I.NE l r [])
  case inst of
    Just x  -> return $ Just $ snd x
    Nothing -> error "Internal error"
buildBinOp _ S.OpLT fl fr = do
  ol     <- procOperand Nothing fl False
  or     <- procOperand Nothing fr False
  (l, r) <- promoteType False ol or
  inst   <- if rankType l > 128
    then emitInst ITLocalOp T.i1 Nothing (FCmp FP.OLT l r [])
    else emitInst ITLocalOp T.i1 Nothing (ICmp I.SLT l r [])
  case inst of
    Just x  -> return $ Just $ snd x
    Nothing -> error "Internal error"
buildBinOp _ S.OpLE fl fr = do
  ol     <- procOperand Nothing fl False
  or     <- procOperand Nothing fr False
  (l, r) <- promoteType False ol or
  inst   <- if rankType l > 128
    then emitInst ITLocalOp T.i1 Nothing (FCmp FP.OLE l r [])
    else emitInst ITLocalOp T.i1 Nothing (ICmp I.SLE l r [])
  case inst of
    Just x  -> return $ Just $ snd x
    Nothing -> error "Internal error"
buildBinOp recov S.OpAND fl fr = do
  ol           <- procOperand recov fl False
  tmp          <- emitInst ITLocalOp T.i1 Nothing (Alloca T.i1 Nothing 1 [])
  currentBlock <- gets currentBlock
  recover      <- addBlock
  falseBlock   <- addBlock
  case tmp of
    Just x -> emitInst
      ITNoOp
      T.i1
      Nothing
      (Store False
             (getOperandPtr (snd x))
             (ConstantOperand $ C.Int 1 0)
             Nothing
             4
             []
      )
    Nothing -> error "Internal error"
  falseBlockState <- getBlock falseBlock
  modifyBlock falseBlock falseBlockState { term = Just $ Do $ Br recover [] }
  truerBlock <- addBlock
  case tmp of
    Just x -> emitInst
      ITNoOp
      T.i1
      Nothing
      (Store False
             (getOperandPtr (snd x))
             (ConstantOperand $ C.Int 1 1)
             Nothing
             4
             []
      )
    Nothing -> error "Internal error"
  truerBlockState <- getBlock truerBlock
  modifyBlock truerBlock truerBlockState { term = Just $ Do $ Br recover [] }
  truelContBlock  <- addBlock
  truelBlock      <- addBlock
  truelBlockState <- getBlock truelBlock
  modifyBlock truelBlock
              truelBlockState { term = Just $ Do $ Br truelContBlock [] }
  or <- procOperand (Just truelContBlock) fr False
  modify $ \s -> s { currentBlock = truelContBlock }
  truelContBlockState <- getBlock truelContBlock
  modifyBlock
    truelContBlock
    truelContBlockState { term = Just $ Do $ CondBr or truerBlock falseBlock []
                        }
  currentBlockState <- getBlock currentBlock
  modifyBlock
    currentBlock
    currentBlockState { term = Just $ Do $ CondBr ol truelBlock falseBlock [] }
  recoverState <- getBlock recover
  nextBlock    <- predictNextBlock
  case recov of
    Just rc -> modifyBlock recover recoverState { term = Just $ Do $ Br rc [] }
    Nothing ->
      modifyBlock recover recoverState { term = Just $ Do $ Br nextBlock [] }
  modify $ \s -> s { currentBlock = recover }
  case tmp of
    Just x -> do
      loadTmp <- emitInst ITLocalOp
                          T.i1
                          Nothing
                          (Load False (getOperandPtr (snd x)) Nothing 4 [])
      case loadTmp of
        Just x  -> return $ Just (snd x)
        Nothing -> error "Internal error"
    Nothing -> error "Internal error"
buildBinOp recov S.OpOR fl fr = do
  ol <- procOperand recov fl False
  emitInst ITLocalOp
           T.i1
           Nothing
           (ICmp I.NE ol (ConstantOperand $ C.Int 1 0) [])
  tmp          <- emitInst ITLocalOp T.i1 Nothing (Alloca T.i1 Nothing 1 [])
  currentBlock <- gets currentBlock
  recover      <- addBlock
  trueBlock    <- addBlock
  case tmp of
    Just x -> emitInst
      ITNoOp
      T.i1
      Nothing
      (Store False
             (getOperandPtr (snd x))
             (ConstantOperand $ C.Int 1 1)
             Nothing
             4
             []
      )
    Nothing -> error "Internal error"
  trueBlockState <- getBlock trueBlock
  modifyBlock trueBlock trueBlockState { term = Just $ Do $ Br recover [] }
  falserBlock <- addBlock
  case tmp of
    Just x -> emitInst
      ITNoOp
      T.i1
      Nothing
      (Store False
             (getOperandPtr (snd x))
             (ConstantOperand $ C.Int 1 0)
             Nothing
             4
             []
      )
    Nothing -> error "Internal error"
  falserBlockState <- getBlock falserBlock
  modifyBlock falserBlock falserBlockState { term = Just $ Do $ Br recover [] }
  falselContBlock  <- addBlock
  falselBlock      <- addBlock
  falselBlockState <- getBlock falselBlock
  modifyBlock falselBlock
              falselBlockState { term = Just $ Do $ Br falselContBlock [] }
  or <- procOperand (Just falselContBlock) fr False
  modify $ \s -> s { currentBlock = falselContBlock }
  falselContBlockState <- getBlock falselContBlock
  modifyBlock
    falselContBlock
    falselContBlockState { term = Just $ Do $ CondBr or trueBlock falserBlock []
                         }
  currentBlockState <- getBlock currentBlock
  modifyBlock
    currentBlock
    currentBlockState { term = Just $ Do $ CondBr ol trueBlock falselBlock [] }
  recoverState <- getBlock recover
  nextBlock    <- predictNextBlock
  case recov of
    Just rc -> modifyBlock recover recoverState { term = Just $ Do $ Br rc [] }
    Nothing ->
      modifyBlock recover recoverState { term = Just $ Do $ Br nextBlock [] }
  modify $ \s -> s { currentBlock = recover }
  case tmp of
    Just x -> do
      loadTmp <- emitInst ITLocalOp
                          T.i1
                          Nothing
                          (Load False (getOperandPtr (snd x)) Nothing 4 [])
      case loadTmp of
        Just x  -> return $ Just (snd x)
        Nothing -> error "Internal error"
    Nothing -> error "Internal error"
buildBinOp _ S.Assign fl fr = do
  ol     <- procOperand Nothing fl True
  or     <- procOperand Nothing fr False
  (l, r) <- promoteType True ol or
  emitInst ITNoOp
           VoidType
           Nothing
           (Store False (getOperandPtr l) r Nothing 4 [])
  return Nothing

isVariable :: S.Expr -> Bool
isVariable S.Var{} = True
isVariable _       = False

getType :: Bool -> S.Expr -> Type
getType p (S.Int _ _) | p     = T.PointerType T.i32 (A.AddrSpace 0)
                      | not p = T.i32
getType p (S.Float _) | p     = T.PointerType T.float (A.AddrSpace 0)
                      | not p = T.float
getType _ S.Var{} = T.PointerType T.i32 (A.AddrSpace 0)
getType _ _       = error "Internal error"

getOperand :: Bool -> S.Expr -> FunctionState (Maybe Operand)
getOperand _ (S.Int b a) =
  return $ Just (ConstantOperand (C.Int (read (show b) :: Word32) a))
getOperand _ (S.Float f) =
  return $ Just (ConstantOperand (C.Float (F.Single (realToFrac f))))
getOperand _     (S.Bool a   ) = return $ Just (ConstantOperand (C.Int 1 a))
getOperand False (S.Var n e p) = do
  st  <- gets symtbl
  gst <- gets gsymtbl
  let varname = mkName n
  objIdx  <- findElementInObjMember varname
  osymtbl <- gets osymtbl
  gets inputs
  inputsIdx <- findArgumentInInputMember varname
  let objType = buildStructure osymtbl
  case inputsIdx of
    Just (x, _) -> return $ Just $ LocalReference x varname
    Nothing     -> case objIdx of
      Just (x, y) -> do
        tmp <- emitInst
          ITLocalOp
          x
          Nothing
          (GetElementPtr
            True
            (LocalReference (PointerType objType (A.AddrSpace 0)) (Name "_self")
            )
            [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 y)]
            []
          )
        case tmp of
          Just (_, m) -> do
            tmpload <- emitInst ITLocalOp
                                (getOperandType m)
                                Nothing
                                (Load False (getOperandPtr m) Nothing 4 [])
            case tmpload of
              Just a -> return $ Just $ snd a
              Nothing ->
                error ("Variable " ++ n ++ " is undefined in " ++ show p)
          Nothing -> error ("Variable " ++ n ++ " is undefined in " ++ show p)
      Nothing -> case Map.lookup varname st of
        Just (x, ts) -> load x ts
        Nothing      -> case Map.lookup varname gst of
          Just (y, ts) -> load y ts
          Nothing -> error ("Variable " ++ n ++ " is undefined in " ++ show p)
 where
  load y ts = do
    tmp_ <- emitInst ITLocalOp
                     (getOperandType y)
                     Nothing
                     (Load False (getOperandPtr y) Nothing 4 [])
    let Just (_, tmp) = tmp_
    if e /= []
      then do
        extObj_ <- findElementInExtObjMember (mkName (Prelude.head e))
                                             (mkName ts) -- FIXME Only one level field will be processed.
        let Just (x, y_) = extObj_
        ptr_ <- emitInst
          ITLocalOp
          x
          Nothing
          (GetElementPtr
            True
            (LocalReference (PointerType (getOperandType y) (A.AddrSpace 0))
                            (mkName n)
            )
            [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 y_)]
            []
          )
        let Just (_, ptr) = ptr_
        val_ <- emitInst ITLocalOp
                         x
                         Nothing
                         (Load False (getOperandPtr ptr) Nothing 4 [])
        let Just (_, val) = val_
        return (Just val)
      else return (Just tmp)
getOperand True (S.Var n _ p) = do
  st  <- gets symtbl
  gst <- gets gsymtbl
  let varname = mkName n
  objIdx  <- findElementInObjMember varname
  osymtbl <- gets osymtbl
  let objType = buildStructure osymtbl
  case objIdx of
    Just (x, y) -> do
      tmp <- emitInst
        ITLocalOp
        x
        Nothing
        (GetElementPtr
          True
          (LocalReference (PointerType objType (A.AddrSpace 0)) (Name "_self"))
          [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 y)]
          []
        )
      case tmp of
        Just m  -> return $ Just $ snd m
        Nothing -> error ("Variable " ++ n ++ " is undefined in " ++ show p)
    Nothing -> case Map.lookup varname st of
      Just (x, _) -> return $ Just x
      Nothing     -> case Map.lookup varname gst of
        Just (y, _) -> return $ Just y
        Nothing     -> error ("Variable " ++ n ++ " is undefined in " ++ show p)
getOperand _ S.BinOp{} = return Nothing
getOperand _ _         = error "Internal error"

findElementInObjMember :: Name -> FunctionState (Maybe (Type, Integer))
findElementInObjMember n = do
  symtbl <- gets osymtbl
  return $ matchNameOfObjMember symtbl 0 n

findElementInExtObjMember
  :: Name -> Name -> FunctionState (Maybe (Type, Integer))
findElementInExtObjMember e s = do
  got <- gets gotypes
  case Map.lookup s got of
    Just x  -> return $ matchNameOfObjMember x 0 e
    Nothing -> error ("Type " ++ show s ++ " not be found")

findArgumentInInputMember :: Name -> FunctionState (Maybe (Type, Integer))
findArgumentInInputMember n = do
  symtbl <- gets inputs
  return $ matchNameOfObjMember symtbl 0 n

matchNameOfObjMember
  :: [(Name, Type)] -> Integer -> Name -> Maybe (Type, Integer)
matchNameOfObjMember (x : xs) i n
  | fst x == n = Just (snd x, i)
  | otherwise  = matchNameOfObjMember xs (i + 1) n
matchNameOfObjMember [] _ _ = Nothing

rankType :: Operand -> Int
rankType a = case getOperandType a of
  T.IntegerType       1          -> 1
  T.IntegerType       8          -> 8
  T.IntegerType       16         -> 16
  T.IntegerType       32         -> 32
  T.IntegerType       64         -> 64
  T.IntegerType       128        -> 128
  T.FloatingPointType T.FloatFP  -> 132
  T.FloatingPointType T.DoubleFP -> 164
  _                              -> 0

promoteType :: Bool -> Operand -> Operand -> FunctionState (Operand, Operand)
promoteType True l r
  | leftRank > rightRank = do
    inst <- extOperand r rightType rightRank leftType leftRank
    case inst of
      Just (x, _) -> return (l, LocalReference leftType x)
      Nothing     -> error "Internal error"
  | leftRank < rightRank = do
    inst <- truncOperand r rightType rightRank leftType leftRank
    case inst of
      Just (x, _) -> return (l, LocalReference leftType x)
      Nothing     -> error "Internal error"
  | otherwise = return (l, r)
 where
  leftType  = getOperandType l

  rightType = getOperandType r

  rightRank = rankType r

  leftRank  = rankType l
promoteType False l r
  | leftRank > rightRank = do
    inst <- extOperand r rightType rightRank leftType leftRank
    case inst of
      Just (x, _) -> return (l, LocalReference leftType x)
      Nothing     -> error "Internal error"
  | leftRank < rightRank = do
    inst <- extOperand l leftType leftRank rightType rightRank
    case inst of
      Just (x, _) -> return (LocalReference rightType x, r)
      Nothing     -> error "Internal error"
  | otherwise = return (l, r)
 where
  leftType  = getOperandType l

  rightType = getOperandType r

  rightRank = rankType r

  leftRank  = rankType l

extOperand
  :: Operand
  -> Type
  -> Int
  -> Type
  -> Int
  -> FunctionState (Maybe (Name, Operand))
extOperand operand _ srcRank targetType typeRank
  | typeRank > 128 && srcRank <= 128 && typeRank - 100 /= srcRank = do
    inst <- emitInst
      ITLocalOp
      (T.IntegerType ((read $ show (typeRank - 100)) :: Word32))
      Nothing
      (SExt operand
            (T.IntegerType ((read $ show (typeRank - 100)) :: Word32))
            []
      )
    case inst of
      Just (_, x) ->
        emitInst ITLocalOp targetType Nothing (SIToFP x targetType [])
      Nothing -> error "Internal error"
  | typeRank - 100 == srcRank = emitInst ITLocalOp
                                         targetType
                                         Nothing
                                         (SIToFP operand targetType [])
  | typeRank > 128 && srcRank > 128 = emitInst ITLocalOp
                                               targetType
                                               Nothing
                                               (FPExt operand targetType [])
  | otherwise = emitInst ITLocalOp
                         targetType
                         Nothing
                         (SExt operand targetType [])

truncOperand
  :: Operand
  -> Type
  -> Int
  -> Type
  -> Int
  -> FunctionState (Maybe (Name, Operand))
truncOperand operand _ srcRank targetType targetRank
  | targetRank > 128 && srcRank <= 128 && targetRank - 100 /= srcRank = do
    inst <- emitInst
      ITLocalOp
      (T.IntegerType ((read $ show (targetRank - 100)) :: Word32))
      Nothing
      (Trunc operand
             (T.IntegerType ((read $ show (targetRank - 100)) :: Word32))
             []
      )
    case inst of
      Just (_, x) ->
        emitInst ITLocalOp targetType Nothing (SIToFP x targetType [])
      Nothing -> error "Internal error"
  | targetRank - 100 == srcRank = emitInst ITLocalOp
                                           targetType
                                           Nothing
                                           (FPTrunc operand targetType [])
  | targetRank > 128 && srcRank > 128 = emitInst
    ITLocalOp
    targetType
    Nothing
    (FPTrunc operand targetType [])
  | otherwise = emitInst ITLocalOp
                         targetType
                         Nothing
                         (Trunc operand targetType [])

getOperandPtr :: Operand -> Operand
getOperandPtr (LocalReference t n) =
  LocalReference (T.PointerType t (A.AddrSpace 0)) n
getOperandPtr (ConstantOperand (C.GlobalReference t n)) =
  ConstantOperand $ C.GlobalReference (T.PointerType t (A.AddrSpace 0)) n
getOperandPtr o = o

getOperandType :: Operand -> Type
getOperandType (LocalReference t _) = t
getOperandType (ConstantOperand (C.GlobalReference t _)) = t
getOperandType (ConstantOperand (C.Int i _)) = T.IntegerType i
getOperandType (ConstantOperand (C.Float _)) = T.FloatingPointType T.FloatFP
getOperandType _ = error "Internal error"

outputLL :: [S.Expr] -> String
outputLL e = L.unpack (ppllvm $ module_ e)
