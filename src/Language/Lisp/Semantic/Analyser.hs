{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Lisp.Semantic.Analyser where

import Control.Monad (unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.State
import Data.Foldable (Foldable (foldr'))
import Data.List.Extra (concatUnzip)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Text qualified as T
import Generics.SYB
import Language.Lisp.Semantic.Type
import Language.Lisp.Type (IdentifierName)

-- | Check if the type is a ListType or a AnyType,
-- use @'@ to indicate the difference between @isListType@ and @isListType'@
isListType' :: LispType -> Bool
isListType' t = t == ListType || t == AnyType

-- | Check if the type is a NumberType or a AnyType,
-- use @'@ to indicate the difference between @isNumber@ and @isNumber'@
isNumber' :: LispType -> Bool
isNumber' t = t == NumberType || t == AnyType

-- | Check if the type is a StringType or a AnyType,
-- use @'@ to indicate the difference between @isString@ and @isString'@
isString' :: LispType -> Bool
isString' t = t == StringType || t == AnyType

-- | Check if the type is a BoolType or a AnyType,
-- use @'@ to indicate the difference between @isBool@ and @isBool'@
isBool' :: LispType -> Bool
isBool' t = t == BoolType || t == AnyType

-- `car` and `cdr` only works for lists.
-- `exit` should call with a number
-- `cons` accepts an item and a list
-- `length` only works for lists
-- `list` only accept lists as arguments
-- Arithmetic Operations only works for numbers
-- `format` only works for strings
typeCheckRule :: SLispProgram -> SemanticAnalyser ()
typeCheckRule = everything (>>) $ mkQ (return ()) check
  where
    check :: SFunctionCall -> SemanticAnalyser ()
    check func =
      mapM_
        (\c -> c func)
        [ checkCar,
          checkCdr,
          checkCons,
          checkLength,
          checkList,
          checkArithmetic,
          checkFormat
        ]

    checkCar :: SFunctionCall -> SemanticAnalyser ()
    checkCar SFunctionCall {..}
      | sFunctionCallName == BuiltinOp LCar =
          unless (all isListType' (Map.elems sFunctionCallArgsType)) $
            throwError $
              TypeError "car only works for list type"
      | otherwise = pure ()

    checkCdr :: SFunctionCall -> SemanticAnalyser ()
    checkCdr SFunctionCall {..}
      | sFunctionCallName == BuiltinOp LCdr =
          unless (all isListType' (Map.elems sFunctionCallArgsType)) $
            throwError $
              TypeError "cdr only works for list type"
      | otherwise = pure ()

    checkCons :: SFunctionCall -> SemanticAnalyser ()
    checkCons SFunctionCall {..}
      | sFunctionCallName == BuiltinOp LCons,
        (_ : t : _) <- Map.elems sFunctionCallArgsType =
          unless (isListType' t) $ throwError $ TypeError "cons only works for list type"
      | otherwise = pure ()

    checkLength :: SFunctionCall -> SemanticAnalyser ()
    checkLength SFunctionCall {..}
      | sFunctionCallName == BuiltinOp LLength,
        [t] <- Map.elems sFunctionCallArgsType =
          unless (isListType' t) $ throwError $ TypeError "length only works for list type"
      | otherwise = pure ()

    checkList :: SFunctionCall -> SemanticAnalyser ()
    checkList SFunctionCall {..}
      | sFunctionCallName == BuiltinOp LList =
          unless (all isListType' (Map.elems sFunctionCallArgsType)) $
            throwError $
              TypeError "list only accept lists as arguments"
      | otherwise = pure ()

    checkArithmetic :: SFunctionCall -> SemanticAnalyser ()
    checkArithmetic SFunctionCall {..}
      | sFunctionCallName `elem` map BuiltinOp arithmeticOpTypeList =
          unless (all isNumber' (Map.elems sFunctionCallArgsType)) $
            throwError $
              TypeError "Arithmetic Operations only works for numbers"
      | otherwise = pure ()

    checkFormat :: SFunctionCall -> SemanticAnalyser ()
    checkFormat SFunctionCall {..}
      | sFunctionCallName == BuiltinOp LFormat =
          unless (all isString' (Map.elems sFunctionCallArgsType)) $
            throwError $
              TypeError "format only works for strings"
      | otherwise = pure ()

-- | Check `if` condition should return bool
ifCheckRule :: SLispProgram -> SemanticAnalyser ()
ifCheckRule = everything (>>) $ mkQ (return ()) checkIf
  where
    checkIf :: SIf -> SemanticAnalyser ()
    checkIf SIf {..} = when (isBool' $ getType sLispIf) $ throwError $ TypeError "if cond needs bool type"

-- | Check `cond` cases should return bool
condCheckRule :: SLispProgram -> SemanticAnalyser ()
condCheckRule = everything (>>) $ mkQ (return ()) checkCond
  where
    checkCond :: SCond -> SemanticAnalyser ()
    checkCond SCond {..} =
      when (all isBool' $ NE.toList $ fmap (getType . fst) sCondCases) $
        throwError $
          TypeError "cond cases need bool type"

type NN = ([IdentifierName], [IdentifierName])

-- | Collect function names and identifier names,
-- return @(function names, identifier names)@
namesInProgram :: SLispProgram -> NN
namesInProgram SLispProgram {..} =
  let go :: [NN -> NN]
      go =
        [ \names -> concatUnzip $ map (`goSFunctionDef` names) sFunctionDefs,
          \names -> concatUnzip $ map (`goSFunctionCall` names) sFunctionCalls,
          \names -> concatUnzip $ map (`goIdentifierDef` names) sIdentifierDefs,
          \names -> concatUnzip $ map (`goLambdaCall` names) sLambdaCalls,
          \names -> concatUnzip $ map (`goIf` names) sIfs,
          \names -> concatUnzip $ map (`goCond` names) sConds
        ]
   in foldr' id mempty go
  where
    goSFunctionDef :: SFunctionDef -> NN -> NN
    goSFunctionDef SFunctionDef {..} (funcs, idents) =
      let names = (sFunctionName : funcs, idents)
       in foldr' goLispExpr names sFunctionBody

    goSFunctionCall :: SFunctionCall -> NN -> NN
    goSFunctionCall SFunctionCall {..} names = foldr' goLispExpr names sFunctionCallArgs

    goIdentifierDef :: SIdentifierDef -> NN -> NN
    goIdentifierDef SIdentifierDef {..} (funcs, idents) =
      let names = (funcs, sIdentifierName : idents)
       in foldr' goLispExpr names (NE.toList sIdentifierBody)

    goLambdaCall :: SLambdaCall -> NN -> NN
    goLambdaCall SLambdaCall {..} names =
      let names' = goLambdaDef sLambdaCall names
       in foldr' goLispExpr names' sLambdaCallArgs

    goLambdaDef :: SLambdaDef -> NN -> NN
    goLambdaDef SLambdaDef {..} names = foldr' goLispExpr names (NE.toList sLambdaBody)

    goIf :: SIf -> NN -> NN
    goIf SIf {..} names =
      let names' = goLispExpr sLispIf names
          names'' = goLispExpr sLispThen names'
       in maybe names'' (`goLispExpr` names'') sLispElse

    goCond :: SCond -> NN -> NN
    goCond SCond {..} names =
      let names' =
            foldr'
              goLispExpr
              names
              (concatMap (\(a, b) -> [a, b]) $ NE.toList sCondCases)
       in maybe names' (`goLispExpr` names') sCondElse

    goLispExpr :: SLispExpr -> NN -> NN
    goLispExpr expr names =
      case expr of
        SLBasic _ -> names
        SLList l -> foldr' goLispExpr names l
        SLFunctionDef f -> goSFunctionDef f names
        SLFunctionCall f -> goSFunctionCall f names
        SLIdentifierDef i -> goIdentifierDef i names
        SLIf i -> goIf i names
        SLLambdaDef l -> goLambdaDef l names
        SLLambdaCall l -> goLambdaCall l names
        SLCond c -> goCond c names
        SLIdentifierCall _ -> names

-- | Check every symbol in the function call has defined before using.
--
-- Check every symbol in the identifier call has defined before using.
scopeCheckRule :: SLispProgram -> SemanticAnalyser ()
scopeCheckRule prog = do
  modify
    ( \env ->
        let (funcs, idents) = namesInProgram prog
         in env {functionNames = funcs, identifierNames = idents}
    )
  checkFunctionCall
  checkIdentifierCall
  checkLispExpr
  where
    checkFunctionCall :: SemanticAnalyser ()
    checkFunctionCall = (everything (>>) $ mkQ (return ()) check) prog
      where
        check :: SFunctionCall -> SemanticAnalyser ()
        check SFunctionCall {..} = do
          names <- gets functionNames
          case sFunctionCallName of
            BuiltinOp _ -> pure ()
            UserDefined name ->
              unless (name `elem` names) $
                throwError $
                  TypeError ("Function call: " <> T.pack (show name) <> " not defined before using")

    checkIdentifierCall :: SemanticAnalyser ()
    checkIdentifierCall = (everything (>>) $ mkQ (return ()) check) prog
      where
        check :: SIdentifierCall -> SemanticAnalyser ()
        check SIdentifierCall {..} = do
          names <- gets identifierNames
          unless (sIdentifierCallName `elem` names) $
            throwError $
              TypeError ("Identifier call: " <> T.pack (show sIdentifierCallName) <> " not defined before using")

    checkLispExpr :: SemanticAnalyser ()
    checkLispExpr = (everything (>>) $ mkQ (return ()) check) prog
      where
        check :: SLispExpr -> SemanticAnalyser ()
        check (SLIdentifierCall name) = do
          names <- gets identifierNames
          unless (name `elem` names) $
            throwError $
              TypeError ("Identifier call: " <> T.pack (show name) <> " not defined before using")
        check _ = pure ()

-- | Check if the function call has the correct number of arguments
argsCheckRule :: SLispProgram -> SemanticAnalyser ()
argsCheckRule prog = do
  checkFunctionCall
  checkLambdaCall
  where
    allFunctionDefs :: Map.Map IdentifierName SFunctionDef
    allFunctionDefs = (everything (<>) $ mkQ Map.empty getFuncDef) prog
      where
        getFuncDef :: SFunctionDef -> Map.Map IdentifierName SFunctionDef
        getFuncDef func = Map.singleton (sFunctionName func) func

    checkFunctionCall :: SemanticAnalyser ()
    checkFunctionCall = (everything (>>) $ mkQ (pure ()) check) prog
      where
        check :: SFunctionCall -> SemanticAnalyser ()
        check SFunctionCall {..} = do
          let argsNumber = length sFunctionCallArgs
              raiseError = throwError $ TypeError $ T.pack (show sFunctionCallName) <> " args number not match"
          case sFunctionCallName of
            BuiltinOp op ->
              case (Map.!?) builtinFuncArgsNumber op of
                Nothing -> pure ()
                Just n -> unless (n == argsNumber) raiseError
            UserDefined op ->
              case (Map.!?) allFunctionDefs op of
                Nothing -> throwError $ TypeError $ T.pack (show sFunctionCallName) <> " has not defined"
                Just func -> unless (length (sFunctionArgs func) == argsNumber) raiseError

        -- TODO: we ignored the condition that has at least n arguments
        builtinFuncArgsNumber :: Map.Map LispBuiltinOp Int
        builtinFuncArgsNumber =
          Map.fromList
            [ (LDisplay, 1),
              (LDisplayLn, 1),
              (LError, 1),
              (LExit, 1),
              (LLoad, 1),
              (LPrint, 1),
              (LAtomQ, 1),
              (LBooleanQ, 1),
              (LIntegerQ, 1),
              (LNumberQ, 1),
              (LListQ, 1),
              (LProcedureQ, 1),
              (LStringQ, 1),
              (LNullQ, 1),
              (LSymbolQ, 1),
              (LCar, 1),
              (LCdr, 1),
              (LCons, 1),
              (LLength, 1),
              (LAbs, 1),
              (LNot, 1),
              (LToNumber, 1),
              (LToString, 1),
              (LToBoolean, 1),
              (LRead, 1),
              (LNth, 2)
            ]

    checkLambdaCall :: SemanticAnalyser ()
    checkLambdaCall = (everything (>>) $ mkQ (pure ()) check) prog
      where
        check :: SLambdaCall -> SemanticAnalyser ()
        check SLambdaCall {..} =
          let lambdaDefArgs = sLambdaArgs sLambdaCall
           in unless
                (length lambdaDefArgs == length sLambdaCallArgs)
                $ throwError
                $ TypeError "lambda call args doesn't match its definition"

runSemanticChecking :: SLispProgram -> SemanticAnalyser ()
runSemanticChecking prog = do
  typeCheckRule prog
  ifCheckRule prog
  condCheckRule prog
  scopeCheckRule prog
  argsCheckRule prog