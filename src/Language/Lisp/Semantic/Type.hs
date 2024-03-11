{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.Lisp.Semantic.Type where

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor (second)
import Data.Data (Data)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Text qualified as T
import GHC.IsList
import GHC.Read (choose)
import Language.Lisp.Type (IdentifierName, LispBasicType (..), getName)
import Language.Lisp.Type qualified as PT
import Text.Read hiding (get)

data LispType
  = -- | Any type means we can't not defer their type at compile time
    AnyType
  | -- | Number type means the value is a number
    NumberType
  | -- | String type means the value is a string
    StringType
  | -- | Bool type means the value is a boolean value
    BoolType
  | -- | List type means the value is a list that contains any type of values
    ListType
  | -- | Function type means the value is a function that takes a value of type t1 and returns a value of type t2
    -- note that the t2 may be also a FunctionType
    FunctionType LispType LispType
  | -- | Unit type means the value is a unit value, which is a value that contains no information
    UnitType
  | -- | MultiParamType means there can be multiple(even infinite) number of the same type
    -- this doesn't contain the return type, for function "+", the signature is @MultiParamType Number -> Number@.
    -- Forthermore, the type in the MultiParamType must be the same, and there at least have one element in the parameter.
    MultiParamType LispType
  | -- | NoneType is used for expression like @(define (f) 3)@, whose type is @FunctionType NoneType NumberType@
    NoneType
  deriving (Eq, Ord, Data)

instance IsList LispType where
  type Item LispType = LispType

  toList AnyType = [AnyType]
  toList NumberType = [NumberType]
  toList StringType = [StringType]
  toList BoolType = [BoolType]
  toList ListType = [ListType]
  toList UnitType = [UnitType]
  toList NoneType = [NoneType]
  toList (FunctionType t1 t2) = toList t1 <> toList t2
  toList (MultiParamType t) = [MultiParamType t]

  fromList [] = error "fromLispTypeList: empty list"
  fromList [t] = t
  fromList (x : xs) = FunctionType x (fromList xs)

instance Show LispType where
  show AnyType = "Any"
  show NumberType = "Number"
  show StringType = "String"
  show BoolType = "Bool"
  show ListType = "List"
  show (FunctionType t1 t2) = show t1 ++ " -> " ++ show t2
  show UnitType = "Unit"
  show (MultiParamType t) = "[" ++ show t ++ "]"
  show NoneType = "None"

data LispBuiltinOp
  = -- | @+ :: [Number] -> Number@
    LAdd
  | -- | @- :: [Number] -> Number@
    LSub
  | -- | @* :: [Number] -> Number@
    LMul
  | -- | @/ :: [Number] -> Number@
    LDiv
  | -- | @% :: [Number] -> Number@
    LRem
  | -- | @and :: [Bool] -> Bool@
    LAnd
  | -- | @or :: [Bool] -> Bool@
    LOr
  | -- | @xor :: [Bool] -> Bool@
    LXor
  | -- | @== :: [Any] -> Bool@, support for both number and string
    LEq
  | -- | @!= :: [Any] -> Bool@, support for both number and string
    LNeq
  | -- | @> :: [Any] -> Bool@, support for both number and string
    LGT
  | -- | @>= :: [Any] -> Bool@, support for both number and string
    LGTE
  | -- | @< :: [Any] -> Bool@, support for both number and string
    LLT
  | -- | @<= :: [Any] -> Bool@, support for both number and string
    LLTE
  | -- | @display :: Any -> Unit@
    LDisplay
  | -- | @displayln :: Any -> Unit@
    LDisplayLn
  | -- | @error :: Any -> Unit@
    LError
  | -- | @exit :: Number -> Unit@
    LExit
  | -- | @Load :: String -> Unit@
    LLoad
  | -- | @print :: Any -> Unit@
    LPrint
  | -- | @atom? :: Any -> Bool@
    LAtomQ
  | -- | @boolean? :: Any -> Bool@
    LBooleanQ
  | -- | @integer? :: Any -> Bool@
    LIntegerQ
  | -- | @number? :: Any -> Bool@
    LNumberQ
  | -- | @list? :: Any -> Bool@
    LListQ
  | -- | @procedure? :: Any -> Bool@
    LProcedureQ
  | -- | @string? :: Any -> Bool@
    LStringQ
  | -- | @null? :: Any -> Bool@
    LNullQ
  | -- | @symbol? :: Any -> Bool@
    LSymbolQ
  | -- | @car :: List -> Any@
    LCar
  | -- | @cdr :: List -> List@
    LCdr
  | -- | @cons :: Any -> List -> List@
    LCons
  | -- | @length :: List -> Number@
    LLength
  | -- | @list :: [List] -> List List@
    LList
  | -- | @append :: [List] -> List@
    LAppend
  | -- | @abs :: Number -> Number@
    LAbs
  | -- | @eq? :: [Any] -> Bool@
    LEqQ
  | -- | @not :: Bool -> Bool@
    LNot
  | -- | @format :: String -> [String] -> String@
    LFormat
  | -- | @to-number :: Any -> Number@
    LToNumber
  | -- | @to-string :: Any -> String@
    LToString
  | -- | @to-boolean :: Any -> Bool@
    LToBoolean
  | -- | @read :: String@
    LRead
  | -- | @nth :: Number -> List -> Any@
    LNth
  | -- | @sqrt :: Number -> Number@
    LSqrt
  deriving (Eq, Show, Ord, Data)

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
        ("<=", return LLTE),
        ("display", return LDisplay),
        ("displayln", return LDisplayLn),
        ("error", return LError),
        ("exit", return LExit),
        ("load", return LLoad),
        ("print", return LPrint),
        ("atom?", return LAtomQ),
        ("boolean?", return LBooleanQ),
        ("integer?", return LIntegerQ),
        ("number?", return LNumberQ),
        ("list?", return LListQ),
        ("procedure?", return LProcedureQ),
        ("string?", return LStringQ),
        ("null?", return LNullQ),
        ("symbol?", return LSymbolQ),
        ("car", return LCar),
        ("cdr", return LCdr),
        ("cons", return LCons),
        ("length", return LLength),
        ("list", return LList),
        ("append", return LAppend),
        ("abs", return LAbs),
        ("eq?", return LEqQ),
        ("not", return LNot),
        ("format", return LFormat),
        ("to-number", return LToNumber),
        ("to-string", return LToString),
        ("to-boolean", return LToBoolean),
        ("read", return LRead),
        ("nth", return LNth),
        ("sqrt", return LSqrt)
      ]

builtinOpList :: (IsString s) => [s]
builtinOpList =
  [ "+",
    "-",
    "*",
    "/",
    "%",
    "and",
    "or",
    "xor",
    "==",
    "!=",
    ">",
    ">=",
    "<",
    "<=",
    "display",
    "displayln",
    "error",
    "exit",
    "load",
    "print",
    "atom?",
    "boolean?",
    "integer?",
    "number?",
    "list?",
    "procedure?",
    "string?",
    "null?",
    "symbol?",
    "car",
    "cdr",
    "cons",
    "length",
    "list",
    "append",
    "abs",
    "eq?",
    "not",
    "format",
    "to-number",
    "to-string",
    "to-boolean",
    "read",
    "nth",
    "sqrt"
  ]

builtinOpTypeList :: [LispBuiltinOp]
builtinOpTypeList = map read builtinOpList

arithmeticOpList :: (IsString s) => [s]
arithmeticOpList =
  [ "+",
    "-",
    "*",
    "/",
    "%",
    "and",
    "or",
    "xor",
    "==",
    "!=",
    ">",
    ">=",
    "<",
    "<=",
    "abs",
    "sqrt"
  ]

arithmeticOpTypeList :: [LispBuiltinOp]
arithmeticOpTypeList = map read arithmeticOpList

data SIdentifierName
  = -- | Builtin function names
    BuiltinOp LispBuiltinOp
  | -- | User defined function names by @define@ keywords
    UserDefined IdentifierName
  deriving (Eq, Show, Ord, Data)

data SLispExpr
  = SLBasic LispBasicType
  | SLList [SLispExpr]
  | SLFunctionDef SFunctionDef
  | SLFunctionCall SFunctionCall
  | SLIdentifierDef SIdentifierDef
  | SLIf SIf
  | SLLambdaDef SLambdaDef
  | SLLambdaCall SLambdaCall
  | SLCond SCond
  | SLIdentifierCall IdentifierName
  deriving (Eq, Show, Ord, Data)

data SCond = SCond
  { -- | Semantic representation of cond cases
    sCondCases :: NE.NonEmpty (SLispExpr, SLispExpr),
    -- | Semantic representation of cond else branch
    sCondElse :: Maybe SLispExpr,
    -- | Semantic representation of cond args type
    sCondArgsType :: Map.Map SLispExpr LispType,
    -- | Semantic representation of cond return type
    sCondReturnType :: LispType
  }
  deriving (Eq, Show, Ord, Data)

data SIf = SIf
  { -- | Semantic if condition
    sLispIf :: SLispExpr,
    -- | Semantic then branch
    sLispThen :: SLispExpr,
    -- | Semantic else branch
    sLispElse :: Maybe SLispExpr,
    -- | Semantic if type
    sIfArgsType :: Map.Map SLispExpr LispType,
    -- | Semantic if return type, for example, @if (> x 0) "positive" "non-positive"@, the return type is @String@
    sIfReturnType :: LispType
  }
  deriving (Eq, Show, Ord, Data)

data SLambdaDef = SLambdaDef
  { -- | Semantic lambda arguments
    sLambdaArgs :: [IdentifierName],
    -- | Semantic lambda body
    sLambdaBody :: NE.NonEmpty SLispExpr,
    -- | Semantic lambda arguments type, for example, @lambda (x y) (* x y)@, the type is @fromList [("x", NumberType), ("y, NumberType)]@.
    sLambdaDefArgsType :: Map.Map SLispExpr LispType,
    -- | Semantic lambda return type
    sLambdaDefType :: LispType
  }
  deriving (Eq, Show, Ord, Data)

data SLambdaCall = SLambdaCall
  { -- | Semantic lambda definition
    sLambdaCall :: SLambdaDef,
    -- | Semantic lambda call arguments
    sLambdaCallArgs :: [SLispExpr],
    -- | Semantic lambda call args type
    sLambdaCallArgsType :: Map.Map SLispExpr LispType,
    -- | Semantic lambda call return type
    sLambdaCallType :: LispType
  }
  deriving (Eq, Show, Ord, Data)

data SIdentifierDef = SIdentifierDef
  { -- | Semantic identifier name
    sIdentifierName :: IdentifierName,
    -- | Semantic identifier body
    sIdentifierBody :: NE.NonEmpty SLispExpr,
    -- | Semantic identifier type
    sIdentifierType :: LispType
  }
  deriving (Eq, Show, Ord, Data)

data SIdentifierCall = SIdentifierCall
  { -- | Semantic Identifier name
    sIdentifierCallName :: IdentifierName,
    -- | Semantic Identifier type
    sIdentifierCallType :: LispType
  }
  deriving (Eq, Show, Ord, Data)

data SFunctionDef = SFunctionDef
  { -- | Semantic function name
    sFunctionName :: IdentifierName,
    -- | Semantic function arguments
    sFunctionArgs :: [IdentifierName],
    -- | Semantic function body
    sFunctionBody :: NE.NonEmpty SLispExpr,
    -- | Semantic function args type
    sFunctionArgsType :: Map.Map SLispExpr LispType,
    -- | Semantic function return type
    sFunctionReturnType :: LispType
  }
  deriving (Eq, Show, Ord, Data)

data SFunctionCall = SFunctionCall
  { -- | Semantic function name
    sFunctionCallName :: SIdentifierName,
    -- | Semantic function call arguments
    sFunctionCallArgs :: [SLispExpr],
    -- | Semantic function call args type
    sFunctionCallArgsType :: Map.Map SLispExpr LispType,
    -- | Semantic function call return type
    sFunctionCallReturnType :: LispType
  }
  deriving (Eq, Show, Ord, Data)

data SLispProgram = SLispProgram
  { -- | Semantic top level function definitions, like @(define (f a b) (+ a b))@
    sFunctionDefs :: [SFunctionDef],
    -- | Semantic top level function calls, like @(f 2 3)@
    sFunctionCalls :: [SFunctionCall],
    -- | Semantic top level identifier definitions, like @(define f (+ 2 3))@ or @(define x 1)@
    sIdentifierDefs :: [SIdentifierDef],
    -- | Semantic top level identifier calls, like @f@ or @x@
    sIdentifierCalls :: [SIdentifierCall],
    -- | Semantic top level lambda definitions, like @(lambda (x) (* x x))@
    sLambdaCalls :: [SLambdaCall],
    -- | Semantic top level if calls, like @(if (> x 0) "positive" "non-positive")@
    sIfs :: [SIf],
    -- | Semantic top level cond calls, like @(cond ((> x 0) "positive") (else "non-positive"))@
    sConds :: [SCond]
  }
  deriving (Eq, Show, Ord, Data)

class HasType a where
  getType :: a -> LispType

instance HasType LispType where
  getType = id

instance HasType LispBasicType where
  getType (LNumber _) = NumberType
  getType (LString _) = StringType
  getType (LBoolean _) = BoolType

instance HasType SLispExpr where
  getType (SLBasic b) = getType b
  getType (SLList _) = AnyType
  getType (SLFunctionDef f) = getType f
  getType (SLFunctionCall f) = getType f
  getType (SLIdentifierDef i) = getType i
  getType (SLIf i) = getType i
  getType (SLLambdaDef l) = getType l
  getType (SLLambdaCall l) = getType l
  getType (SLCond c) = getType c
  getType (SLIdentifierCall _) = AnyType

instance HasType SCond where
  getType = sCondReturnType

instance HasType SIf where
  getType = sIfReturnType

instance HasType SLambdaDef where
  getType SLambdaDef {..} = fromList $ Map.elems sLambdaDefArgsType <> [sLambdaDefType]

instance HasType SLambdaCall where
  getType = sLambdaCallType

instance HasType SIdentifierDef where
  getType = sIdentifierType

instance HasType SIdentifierCall where
  getType = sIdentifierCallType

instance HasType SFunctionDef where
  getType SFunctionDef {..} = fromList $ Map.elems sFunctionArgsType <> [sFunctionReturnType]

instance HasType SFunctionCall where
  getType = sFunctionCallReturnType

instance HasType LispBuiltinOp where
  getType LAdd = mnn
  getType LSub = mnn
  getType LMul = mnn
  getType LDiv = mnn
  getType LRem = mnn
  getType LAnd = mbb
  getType LOr = mbb
  getType LXor = mbb
  getType LEq = mab
  getType LNeq = mab
  getType LGT = mab
  getType LGTE = mab
  getType LLT = mab
  getType LLTE = mab
  getType LDisplay = au
  getType LDisplayLn = au
  getType LError = au
  getType LExit = nu
  getType LLoad = su
  getType LPrint = au
  getType LAtomQ = ab
  getType LBooleanQ = ab
  getType LIntegerQ = ab
  getType LNumberQ = ab
  getType LListQ = ab
  getType LProcedureQ = ab
  getType LStringQ = ab
  getType LNullQ = ab
  getType LSymbolQ = ab
  getType LCar = la
  getType LCdr = ll
  getType LCons = FunctionType AnyType ll
  getType LLength = ln
  getType LList = mll
  getType LAppend = mll
  getType LAbs = nn
  getType LEqQ = mab
  getType LNot = bb
  getType LFormat = FunctionType StringType (MultiParamType StringType)
  getType LToNumber = FunctionType AnyType NumberType
  getType LToString = FunctionType AnyType StringType
  getType LToBoolean = FunctionType AnyType BoolType
  getType LRead = FunctionType UnitType StringType
  getType LNth = FunctionType NumberType (FunctionType ListType AnyType)
  getType LSqrt = nn

-- | [Number] -> Number
mnn :: LispType
mnn = FunctionType (MultiParamType NumberType) NumberType

-- | [Bool] -> Bool
mbb :: LispType
mbb = FunctionType (MultiParamType BoolType) BoolType

-- | [Any] -> Bool
mab :: LispType
mab = FunctionType (MultiParamType AnyType) BoolType

-- | [List] -> List
mll :: LispType
mll = FunctionType (MultiParamType ListType) ListType

-- | AnyType -> Unit
au :: LispType
au = FunctionType AnyType UnitType

-- | Number -> Unit
nu :: LispType
nu = FunctionType NumberType UnitType

-- | String -> Bool
su :: LispType
su = FunctionType StringType UnitType

-- | Any -> Bool
ab :: LispType
ab = FunctionType AnyType BoolType

-- | Bool -> Bool
bb :: LispType
bb = FunctionType BoolType BoolType

-- | List -> Any
la :: LispType
la = FunctionType ListType AnyType

-- | List -> List
ll :: LispType
ll = FunctionType ListType ListType

-- | List List -> List
ln :: LispType
ln = FunctionType ListType NumberType

-- | Number -> Number
nn :: LispType
nn = FunctionType NumberType NumberType

data SemanticEnv = SemanticEnv
  { typeMap :: Map.Map SLispExpr LispType,
    -- | function define names
    functionNames :: [PT.IdentifierName],
    -- | identifier define names
    identifierNames :: [PT.IdentifierName]
  }
  deriving (Eq, Show)

instance Semigroup SemanticEnv where
  (SemanticEnv a b c) <> (SemanticEnv a' b' c') = SemanticEnv (a <> a') (b <> b') (c <> c')

instance Monoid SemanticEnv where
  mempty = SemanticEnv mempty mempty mempty

-- | Type inference error
data TypeError = TypeError T.Text
  deriving (Eq, Show)

type SemanticAnalyser = StateT SemanticEnv (ExceptT TypeError IO)

-- AST -> SAST
class ToSemantic a b | a -> b where
  toSemantic :: a -> SemanticAnalyser b

instance ToSemantic PT.LispProgram SLispProgram where
  toSemantic PT.LispProgram {..} = do
    sFunctionDefs <- traverse toSemantic functionDefs
    sFunctionCalls <- traverse toSemantic functionCalls
    sIdentifierDefs <- traverse toSemantic identifierDefs
    sIdentifierCalls <- traverse toSemantic identifierCalls
    sLambdaCalls <- traverse toSemantic lambdaCalls
    sIfs <- traverse toSemantic ifs
    sConds <- traverse toSemantic conds
    pure $ SLispProgram {..}

instance ToSemantic PT.FunctionDef SFunctionDef where
  toSemantic (PT.FunctionDef name args bodies) = do
    when (name `elem` builtinOpList) $ throwError $ TypeError $ T.pack "Function name conflict with builtin function"

    env <- gets typeMap
    sbodies <- traverse toSemantic bodies
    modify (\s -> s {typeMap = env})
    let argsType = map (\arg -> Map.findWithDefault AnyType (SLIdentifierCall arg) env) args
        funcType = getType $ NE.last sbodies
        argsTypeMap = fromList $ zip (map SLIdentifierCall args) argsType
    pure $ SFunctionDef name args sbodies argsTypeMap funcType

-- | Note: Only function call can infer its type at compile time
instance ToSemantic PT.FunctionCall SFunctionCall where
  toSemantic (PT.FunctionCall name args) = do
    env <- gets typeMap

    sargs <- traverse toSemantic args
    let argsTypes = map getType sargs
        argsTypeMap = fromList $ zip sargs argsTypes

    case readMaybe $ T.unpack $ getName name of
      -- If the function is a user defined function, we can't infer its type at semantic analysis time
      Nothing -> pure $ SFunctionCall (UserDefined name) sargs argsTypeMap AnyType
      Just op -> do
        let opType = getType op
            identNames = allIdentifierName sargs
        -- If the function is a builtin function, we can infer its type at semantic analysis time
        -- If the type matchs the builtin function, we need to update the typeMap and return the SFunctionCall
        if argsTypeMatch opType argsTypes
          then do
            modify (\s -> s {typeMap = argsTypeMap <> env})
            pure $ SFunctionCall (BuiltinOp op) sargs argsTypeMap (getReturnType opType)
          else -- if the args are all IdentifierName, we need to update the corresponding typeMap

            if length identNames == length sargs
              then do
                let newTypeMap = buildTypeMap opType sargs
                modify (\s -> s {typeMap = newTypeMap <> env})
                pure $ SFunctionCall (BuiltinOp op) sargs argsTypeMap (getReturnType opType)
              else throwError $ TypeError $ T.pack "SFunctionCall: Type mismatch"
    where
      allIdentifierName :: [SLispExpr] -> [SLispExpr]
      allIdentifierName [] = []
      allIdentifierName (name@(SLIdentifierCall _) : xs) = name : allIdentifierName xs
      allIdentifierName (_ : xs) = xs

      buildTypeMap :: LispType -> [SLispExpr] -> Map.Map SLispExpr LispType
      buildTypeMap (FunctionType (MultiParamType mt) _) args = fromList $ map (,mt) args
      buildTypeMap (FunctionType a b) (x : xs) = Map.insert x a $ buildTypeMap b xs
      buildTypeMap a [b] = Map.singleton b a
      buildTypeMap _ _ = error "buildTypeMap: Type mismatch"

instance ToSemantic PT.IdentifierDef SIdentifierDef where
  toSemantic (PT.IdentifierDef name bodies) = do
    when (name `elem` builtinOpList) $ throwError $ TypeError $ T.pack "Identifier name conflict with builtin function"
    sbodies <- traverse toSemantic bodies
    let identType = getType $ NE.last sbodies
    pure $ SIdentifierDef name sbodies identType

instance ToSemantic PT.IdentifierCall SIdentifierCall where
  toSemantic (PT.IdentifierCall name) = do
    env <- gets typeMap
    let identType = Map.findWithDefault AnyType (SLIdentifierCall name) env
    pure $ SIdentifierCall name identType

instance ToSemantic PT.LambdaDef SLambdaDef where
  toSemantic (PT.LambdaDef args bodies) = do
    env <- gets typeMap
    sbodies <- traverse toSemantic bodies
    let argsType = map (\arg -> Map.findWithDefault AnyType (SLIdentifierCall arg) env) args
        lambdaType = getType $ NE.last sbodies
        argsTypeMap = fromList $ zip (map SLIdentifierCall args) argsType
    pure $ SLambdaDef args sbodies argsTypeMap lambdaType

instance ToSemantic PT.LambdaCall SLambdaCall where
  toSemantic (PT.LambdaCall lambdaDef args) = do
    sLambdaDef <- toSemantic lambdaDef
    sargs <- traverse toSemantic args
    let argsType = map getType sargs
        argsTypeMap = fromList $ zip sargs argsType
        lambdaType = getType sLambdaDef
    pure $ SLambdaCall sLambdaDef sargs argsTypeMap lambdaType

instance ToSemantic PT.If SIf where
  toSemantic (PT.If cond thenBranch elseBranch) = do
    scond <- toSemantic cond
    sthenBranch <- toSemantic thenBranch
    selseBranch <- maybe (pure Nothing) (fmap Just . toSemantic) elseBranch

    let condType = getType scond
        thenType = getType sthenBranch
        elseType = getType <$> selseBranch
    if typeMatch condType BoolType
      then do
        let returnType = fromMaybe thenType elseType
        pure $
          SIf
            scond
            sthenBranch
            selseBranch
            (Map.fromList [(scond, condType), (sthenBranch, thenType)] <> foldMap (`Map.singleton` thenType) selseBranch)
            returnType
      else throwError $ TypeError $ T.pack "SIf: Type mismatch"

instance ToSemantic PT.Cond SCond where
  toSemantic (PT.Cond cases elseBranch) = do
    scases <- traverse (\(a, b) -> (,) <$> toSemantic a <*> toSemantic b) cases
    selseBranch <- maybe (pure Nothing) (fmap Just . toSemantic) elseBranch

    let argsType = Map.fromList $ map (second getType) (NE.toList scases)
        returnType = getType $ fromMaybe (snd $ NE.last scases) selseBranch
    pure $ SCond scases selseBranch argsType returnType

instance ToSemantic PT.LispExpr SLispExpr where
  toSemantic (PT.LBasic b) = SLBasic <$> toSemantic b
  toSemantic (PT.LList l) = SLList <$> traverse toSemantic l
  toSemantic (PT.LFunctionDef f) = SLFunctionDef <$> toSemantic f
  toSemantic (PT.LFunctionCall f) = SLFunctionCall <$> toSemantic f
  toSemantic (PT.LIdentifierDef i) = SLIdentifierDef <$> toSemantic i
  toSemantic (PT.LIf i) = SLIf <$> toSemantic i
  toSemantic (PT.LLambdaDef l) = SLLambdaDef <$> toSemantic l
  toSemantic (PT.LLambdaCall l) = SLLambdaCall <$> toSemantic l
  toSemantic (PT.LCond c) = SLCond <$> toSemantic c
  toSemantic (PT.LIdentifier i) = pure $ SLIdentifierCall i

instance ToSemantic LispBasicType LispBasicType where
  toSemantic = pure

-- | @typeMatch expected real@ returns @True@ if @real@ is equal to @expected@, otherwise @False@.
-- Note, AnyType is equal to Anytype.
typeMatch :: LispType -> LispType -> Bool
typeMatch (MultiParamType mt) t =
  multiParamTypeMatch mt t
  where
    multiParamTypeMatch mt (FunctionType cur others) = mt == AnyType || cur == AnyType || (mt == cur && multiParamTypeMatch mt others)
    multiParamTypeMatch mt t = t == AnyType || mt == AnyType || mt == t
typeMatch a b = a == AnyType || b == AnyType || a == b

-- | @argsTypeMatch expected real@ returns @True@ if @real@ is equal to @expected@'s args type, otherwise @False@.
argsTypeMatch :: LispType -> [LispType] -> Bool
argsTypeMatch t1 t2 = typeMatch (dropReturnType t1) (fromList t2)
  where
    dropReturnType :: LispType -> LispType
    dropReturnType (FunctionType res t) = if isFunctionType t then dropReturnType t else res
    dropReturnType a = a

    isFunctionType :: LispType -> Bool
    isFunctionType (FunctionType _ _) = True
    isFunctionType _ = False

getReturnType :: LispType -> LispType
getReturnType (FunctionType _ t) = getReturnType t
getReturnType t = t