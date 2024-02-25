module Language.Lisp.Semantic.Type where

import Data.List.NonEmpty qualified as NE
import GHC.Read (choose)
import Language.Lisp.Type (IdentifierName, LispBasicType (..))
import Text.Read

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
  deriving (Eq)

instance Show LispType where
  show AnyType = "Any"
  show NumberType = "Number"
  show StringType = "String"
  show BoolType = "Bool"
  show ListType = "List"
  show (FunctionType t1 t2) = show t1 ++ " -> " ++ show t2
  show UnitType = "Unit"
  show (MultiParamType t) = "[" ++ show t ++ "]"

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
        ("nth", return LNth)
      ]

data SIdentifierName
  = -- | Builtin function names
    BuiltinOp LispBuiltinOp
  | -- | User defined function names by @define@ keywords
    UserDefined IdentifierName
  deriving (Eq, Show)

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
  | SLIdentifierCall SIdentifierCall
  deriving (Eq, Show)

data SCond = SCond
  { -- | Semantic representation of cond cases
    sCondCases :: NE.NonEmpty (SLispExpr, SLispExpr),
    -- | Semantic representation of cond else branch
    sCondElse :: Maybe SLispExpr,
    -- | Semantic representation of cond type
    sCondType :: LispType
  }
  deriving (Eq, Show)

data SIf = SIf
  { -- | Semantic if condition
    sLispIf :: SLispExpr,
    -- | Semantic then branch
    sLispThen :: SLispExpr,
    -- | Semantic else branch
    sLispElse :: Maybe SLispExpr,
    -- | Semantic if type
    sIfType :: LispType
  }
  deriving (Eq, Show)

data SLambdaDef = SLambdaDef
  { -- | Semantic lambda arguments
    sLambdaArgs :: [IdentifierName],
    -- | Semantic lambda body
    sLambdaBody :: NE.NonEmpty SLispExpr,
    -- | Semantic lambda type
    sLambdaDefType :: LispType
  }
  deriving (Eq, Show)

data SLambdaCall = SLambdaCall
  { -- | Semantic lambda definition
    sLambdaCall :: SLambdaDef,
    -- | Semantic lambda call arguments
    sLambdaCallArgs :: [SLispExpr],
    -- | Semantic lambda call type
    sLambdaCallType :: LispType
  }
  deriving (Eq, Show)

data SIdentifierDef = SIdentifierDef
  { -- | Semantic identifier name
    sIdentifierName :: SIdentifierName,
    -- | Semantic identifier body
    sIdentifierBody :: NE.NonEmpty SLispExpr,
    -- | Semantic identifier type
    sIdentifierType :: LispType
  }
  deriving (Eq, Show)

data SIdentifierCall = SIdentifierCall
  { -- | Semantic Identifier name
    sIdentifierCallName :: SIdentifierName,
    -- | Semantic Identifier type
    sIdentifierCallType :: LispType
  }
  deriving (Eq, Show)

data SFunctionDef = SFunctionDef
  { -- | Semantic function name
    sFunctionName :: IdentifierName,
    -- | Semantic function arguments
    sFunctionArgs :: [IdentifierName],
    -- | Semantic function body
    sFunctionBody :: NE.NonEmpty SLispExpr,
    -- | Semantic function type
    sFunctionType :: LispType
  }
  deriving (Eq, Show)

data SFunctionCall = SFunctionCall
  { -- | Semantic function name
    sFunctionCallName :: SIdentifierName,
    -- | Semantic function call arguments
    sFunctionCallArgs :: [SLispExpr],
    -- | Semantic function call type
    sFunctionCallType :: LispType
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  -- let t1 = getType sLispThen
  --     t2 = getType <$> sLispElse
  --  in if isNothing t2
  --       then AnyType
  --       else maybe AnyType (\t -> if t == t1 then t else AnyType) t2
  getType (SLLambdaDef l) = getType l
  getType (SLLambdaCall l) = getType l
  getType (SLCond c) = getType c
  -- We make this identifier as AnyType since we will evaluate its type later by reference its context
  getType (SLIdentifierCall i) = getType i

instance HasType SCond where
  getType = sCondType

instance HasType SIf where
  getType = sIfType

instance HasType SLambdaDef where
  getType = sLambdaDefType

instance HasType SLambdaCall where
  getType = sLambdaCallType

instance HasType SIdentifierDef where
  getType = sIdentifierType

instance HasType SIdentifierCall where
  getType = sIdentifierCallType

instance HasType SFunctionDef where
  getType = sFunctionType

instance HasType SFunctionCall where
  getType = sFunctionCallType

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
  getType LRead = StringType -- TODO: It's not a trival string
  getType LNth = FunctionType NumberType (FunctionType ListType AnyType)

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