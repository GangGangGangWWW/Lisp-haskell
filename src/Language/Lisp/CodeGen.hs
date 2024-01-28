{-# LANGUAGE OverloadedRecordDot #-}

module Language.Lisp.CodeGen where

import Control.Monad (forM_)
import Control.Monad.Except (ExceptT)
import Control.Monad.State
import Data.Char
import Data.Map qualified as Map
import Data.Text qualified as T
-- import LLVM.AST qualified as LA
-- import LLVM.AST.Type qualified as LA
-- import LLVM.IRBuilder qualified as L
import Language.Lisp.Type

{-
type CodeGenModule a = StateT LA.Module (ExceptT T.Text IO) a

data Env = Env
  { operands :: Map.Map T.Text LA.Operand,
    strings :: Map.Map T.Text LA.Operand
  }
  deriving (Eq, Show)

type LLVM = L.ModuleBuilderT (State Env)

type CodeGen = L.IRBuilderT LLVM

registerOperand :: (MonadState Env m) => T.Text -> LA.Operand -> m ()
registerOperand name op = modify $ \env -> env {operands = Map.insert name op env.operands}

exprToLLVMType :: (MonadState Env m) => LispExpr -> m LA.Type
exprToLLVMType = undefined

{-

codeGenExpr :: LispExpr -> CodeGen LA.Operand
codeGenExpr (LNumber n) = pure $ L.double n
codeGenExpr (LString s) = do
  strs <- gets strings
  case Map.lookup s strs of
    Just op -> pure op
    Nothing -> do
      let nm = LA.mkName $ show (Map.size strs) <> ".str"
      op <- LA.ConstantOperand <$> L.globalStringPtr (T.unpack s) nm
      modify $ \env -> env {strings = Map.insert s op strs}
      pure op
codeGenExpr (LList els) = do
  ops <- mapM codeGenExpr els
  let arrayType = LA.ArrayType (fromIntegral $ length els) LA.i32
  addr <- L.alloca arrayType Nothing 0
  forM_ (zip [0 ..] ops) $ \(i, op) -> do
    ptr <- L.gep LA.i32 addr [L.int32 0, L.int32 i]
    L.store ptr 0 op
  pure addr
codeGenExpr (LIdentifier s) = do
  nm <- L.fresh
  LA.ConstantOperand <$> L.globalStringPtr (T.unpack s) nm

-- codeGenExpr (LFunctionDef name args body) = do
--   let funcName = LA.mkName $ T.unpack name
--   let funcArgs = map (LA.mkName . T.unpack) args
--   let funcType = LA.FunctionType LA.void (map llvmType body) False
--   func <- L.function funcType funcName $ \_ -> do
--     forM_ body codeGenExpr
--     L.retVoid
--   pure $ LA.ConstantOperand $ LA.GlobalReference (LA.ptr funcType) funcName

llvmType :: LispExpr -> LA.Type
llvmType (LNumber _) = LA.double
llvmType (LString s) = LA.VectorType (fromIntegral $ T.length s) LA.i8
llvmType (LList v) = LA.VectorType (fromIntegral $ length v) LA.ptr
llvmType (LIdentifier _) = LA.void
llvmType (LFunctionDef {}) = LA.void
llvmType (LFunctionCall _ _) = LA.void
llvmType (LBoolean _) = LA.i1
llvmType (LIf {}) = LA.void
llvmType (LQuote _) = LA.void
llvmType LNil = LA.void
llvmType (LLambda _ _) = LA.void
llvmType (LLambdaCall _ _) = LA.void
llvmType (LCond _) = LA.void
llvmType (LVariableDef _ _) = LA.void
llvmType _ = undefined

-}

lispHsASTToIR :: LispExpr -> CodeGenModule ()
lispHsASTToIR = undefined

compile :: LA.Module -> FilePath -> IO ()
compile = undefined
-}