{-# LANGUAGE DerivingStrategies #-}

module Language.Lisp.Type where

import Data.List.NonEmpty qualified as NE
import Data.String (IsString)
import Data.Text qualified as T
import GHC.Read (choose)
import Text.Read

type SourceCode = T.Text

type LispNumber = Double

type LispString = T.Text

type Program = [LispExpr]

data LispBuiltinOp
  = -- | @+@
    LAdd
  | -- | @-@
    LSub
  | -- | @*@
    LMul
  | -- | @/@
    LDiv
  | -- | @%@
    LRem
  | -- | @and@
    LAnd
  | -- | @or@
    LOr
  | -- | @xor@
    LXor
  | -- | @==@
    LEq
  | -- | @!=@
    LNeq
  | -- | @>@
    LGT
  | -- | @>=@
    LGTE
  | -- | @<@
    LLT
  | -- | @<=@
    LLTE
  deriving (Eq, Show)

instance Read LispBuiltinOp where
  readPrec =
    choose
      [ ("+", return LAdd),
        ("-", return LSub),
        ("*", return LMul),
        ("/", return LDiv),
        ("%", return LRem),
        ("and", return LAnd),
        ("or", return LOr),
        ("xor", return LXor),
        ("==", return LEq),
        ("!=", return LNeq),
        (">", return LGT),
        (">=", return LGTE),
        ("<", return LLT),
        ("<=", return LLTE)
      ]

-- data LispExpr
-- = -- | Example: @42@
--   LNumber LispNumber
-- \| -- | Example: @"hello"@
--   LString LispString
-- \| -- | Example: @'(1 2 3)@
--   LList [LispExpr]
-- \| -- | Example: @x@
--   LIdentifier LispString
-- \| -- | Example: @(define (add a b) (+ a b))@
--   LFunctionDef
--     -- | Should be LIdentitier only
--     LispExpr
--     -- | Should be LIdentitier only
--     [LispExpr]
--     [LispExpr]
-- \| -- | Example: @(add 3 4)@
--   LFunctionCall
--     -- | Should be LIdentitier only
--     LispExpr
--     [LispExpr]
-- \| -- | Example: @#t@, @#f@
--   LBoolean Bool
-- \| -- | Example: @(if (> x 0) "positive" "non-positive")@
--   LIf LispExpr LispExpr LispExpr
-- \| -- | Example: @'x@
--   LQuote
--     -- | Should be LIdentitier only
--     LispExpr
-- \| -- | Example: @nil@
--   LNil
-- \| -- | Example: @(lambda (x) (* x x))@
--   LLambda
--     -- | Should be LIdentitier only
--     [LispExpr]
--     [LispExpr]
-- \| -- | Example: @((lambda (x) (* x x)) 3)@
--   LLambdaCall LispExpr [LispExpr]
-- \| -- | Example: @(cond ((> x 0) "positive") (else "non-positive"))@,
--   -- we don't allow cond without statements like single @(cond)@ only
--   LCond (NE.NonEmpty (LispExpr, LispExpr))
-- \| -- | Example: @(define x 3)@ or @(define x (+ 1 2))@
--   LVariableDef LispExpr [LispExpr]
-- \| LBuiltin BuiltinOp
-- deriving (Eq, Show)

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

data FunctionCallName
  = -- | Builtin function names
    BuiltinOp LispBuiltinOp
  | -- | User defined function names by @define@ keywords
    UserDefined IdentifierName
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
    functionCallName :: FunctionCallName,
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
