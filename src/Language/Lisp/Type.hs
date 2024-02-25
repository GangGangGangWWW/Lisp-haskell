{-# LANGUAGE DerivingStrategies #-}

module Language.Lisp.Type where

import Data.List.NonEmpty qualified as NE
import Data.String (IsString)
import Data.Text qualified as T

type SourceCode = T.Text

type LispNumber = Double

type LispString = T.Text

type Program = [LispExpr]

data LispBasicType
  = -- | Example: @42@
    LNumber LispNumber
  | -- | Example: @"hello"@
    LString LispString
  | -- | Example: @#t@, @#f@
    LBoolean Bool
  deriving (Eq, Show)

data LispExpr
  = LBasic LispBasicType
  | LList [LispExpr]
  | LFunctionDef FunctionDef
  | LFunctionCall FunctionCall
  | LIdentifierDef IdentifierDef
  | LIf If
  | LLambdaDef LambdaDef
  | LLambdaCall LambdaCall
  | LCond Cond
  | LIdentifier IdentifierName
  deriving (Eq, Show)

newtype IdentifierName = IdentifierName LispString
  deriving newtype (Show)
  deriving (Eq, IsString)

data Cond = Cond
  { -- | Cond cases
    condCases :: NE.NonEmpty (LispExpr, LispExpr),
    -- | Cond else branch
    condElse :: Maybe LispExpr
  }
  deriving (Eq, Show)

data If = If
  { -- | If condition
    lispIf :: LispExpr,
    -- | Then branch
    lispThen :: LispExpr,
    -- | Else branch
    lispElse :: Maybe LispExpr
  }
  deriving (Eq, Show)

data LambdaDef = LambdaDef
  { -- | Lambda arguments
    lambdaArgs :: [IdentifierName],
    -- | Lambda body
    lambdaBody :: NE.NonEmpty LispExpr
  }
  deriving (Eq, Show)

data LambdaCall = LambdaCall
  { -- | Lambda definition
    lambdaCall :: LambdaDef,
    -- | Lambda call arguments
    lambdaCallArgs :: [LispExpr]
  }
  deriving (Eq, Show)

data IdentifierDef = IdentifierDef
  { -- | Identifier name
    identifierName :: IdentifierName,
    -- | Identifier body
    identifierBody :: NE.NonEmpty LispExpr
  }
  deriving (Eq, Show)

newtype IdentifierCall = IdentifierCall
  { -- | Identifier name
    identifierCallName :: IdentifierName
  }
  deriving (Eq, Show)

data FunctionDef = FunctionDef
  { -- | Function name
    functionName :: IdentifierName,
    -- | Function arguments
    functionArgs :: [IdentifierName],
    -- | Function body
    functionBody :: NE.NonEmpty LispExpr
  }
  deriving (Eq, Show)

data FunctionCall = FunctionCall
  { -- | Function name
    functionCallName :: IdentifierName,
    -- | Function call arguments
    functionCallArgs :: [LispExpr]
  }
  deriving (Eq, Show)

data LispProgram = LispProgram
  { -- | Top level function definitions, like @(define (f a b) (+ a b))@
    functionDefs :: [FunctionDef],
    -- | Top level function calls, like @(f 2 3)@
    functionCalls :: [FunctionCall],
    -- | Top level identifier definitions, like @(define f (+ 2 3))@ or @(define x 1)@
    identifierDefs :: [IdentifierDef],
    -- | Top level identifier calls, like @f@ or @x@
    identifierCalls :: [IdentifierCall],
    -- | Top level lambda calls, like @((lambda (x) (* x x)) 3)@
    lambdaCalls :: [LambdaCall],
    -- | Top level if calls, like @(if (> x 0) "positive" "non-positive")@
    ifs :: [If],
    -- | Top level cond calls, like @(cond ((> x 0) "positive") (else "non-positive"))@
    conds :: [Cond]
  }
  deriving (Eq, Show)
