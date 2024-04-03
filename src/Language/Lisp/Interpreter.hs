{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Lisp.Interpreter where

import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (traverse_)
import Data.Foldable.Extra (foldlM)
import Data.List (foldl1')
import Data.List.Extra (product', sum')
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Text qualified as T
import Language.Lisp.Semantic.Type
import Language.Lisp.Type (IdentifierName (getName), LispBasicType (..), LispNumber)
import System.Exit (ExitCode (ExitFailure), exitWith)
import Text.Printf (IsChar (..), printf)
import Text.Read (readMaybe)

type InterpreterError = String

data InterpreterEnv = InterpreterEnv
  { functions :: Map.Map IdentifierName SFunctionDef,
    identifiers :: Map.Map IdentifierName SIdentifierDef,
    -- | Store local variable vaules that are used in function calls.
    -- For example, for the function call @(f a b)@, the local variable values are @(a, v1)@ and @(b, v2)@,
    -- both will be stored in @functionCallStack@ until the @f@ call finished
    functionCallStack :: Map.Map IdentifierName ResultType
  }
  deriving (Show, Eq)

instance Semigroup InterpreterEnv where
  InterpreterEnv f1 i1 s1 <> InterpreterEnv f2 i2 s2 =
    InterpreterEnv (f1 <> f2) (i1 <> i2) (s1 <> s2)

instance Monoid InterpreterEnv where
  mempty = InterpreterEnv mempty mempty mempty

data ResultType
  = ResEmpty
  | ResBasic LispBasicType
  | ResList [ResultType]
  | ResFunction SFunctionDef
  | ResIdentifier SIdentifierDef
  | ResLambda SLambdaDef
  deriving (Eq, Ord)

instance Show ResultType where
  show ResEmpty = ""
  show (ResBasic res) = case res of
    LNumber n -> show n
    LBoolean b -> show b
    LString s -> T.unpack s
  show (ResList items) = "(" <> unwords (map show items) <> ")"
  show (ResFunction _) = "<function>"
  show (ResIdentifier _) = "<identifier>"
  show (ResLambda _) = "<lambda>"

instance IsChar ResultType where
  fromChar = ResBasic . LString . T.singleton
  toChar = \case
    ResBasic (LString c) -> T.head c
    _ -> error "Not a char"

type Interpreter a = StateT InterpreterEnv (ExceptT InterpreterError IO) a

runInterpreter :: SLispProgram -> Interpreter ResultType
runInterpreter SLispProgram {..} = do
  traverse_ runFuntionDef sFunctionDefs
  traverse_ runIdentifierDef sIdentifierDefs
  traverse_ runFunctionCall sFunctionCalls
  traverse_ runIdentifierCall sIdentifierCalls
  traverse_ runLambdaCall sLambdaCalls
  traverse_ runIf sIfs
  traverse_ runCond sConds
  pure ResEmpty

runFuntionDef :: SFunctionDef -> Interpreter ResultType
runFuntionDef func = do
  modify (\s -> s {functions = (\(k, v) -> Map.insert k v (functions s)) (buildFunctionDefPair func)})
  pure ResEmpty

runIdentifierDef :: SIdentifierDef -> Interpreter ResultType
runIdentifierDef ident = do
  modify (\s -> s {identifiers = (\(k, v) -> Map.insert k v (identifiers s)) (buildIdentifierDefPair ident)})
  pure ResEmpty

runFunctionCall :: SFunctionCall -> Interpreter ResultType
runFunctionCall SFunctionCall {..} =
  case sFunctionCallName of
    BuiltinOp LAdd -> do
      args <- traverse runLispExprReturnNumber sFunctionCallArgs
      pure $ ResBasic $ LNumber $ sum' args
    BuiltinOp LSub -> do
      args <- traverse runLispExprReturnNumber sFunctionCallArgs
      pure $ ResBasic $ LNumber $ foldl1' (-) args
    BuiltinOp LMul -> do
      args <- traverse runLispExprReturnNumber sFunctionCallArgs
      pure $ ResBasic $ LNumber $ product' args
    BuiltinOp LDiv -> do
      args <- traverse runLispExprReturnNumber sFunctionCallArgs
      pure $ ResBasic $ LNumber $ foldl1' (/) args
    BuiltinOp LRem -> do
      args <- traverse runLispExprReturnNumber sFunctionCallArgs
      let ints :: [Integer] = map floor args
      pure $ ResBasic $ LNumber $ fromInteger $ foldl1' rem ints
    BuiltinOp LAnd -> do
      args <- traverse runLispExprReturnBool sFunctionCallArgs
      pure $ ResBasic $ LBoolean $ and args
    BuiltinOp LOr -> do
      args <- traverse runLispExprReturnBool sFunctionCallArgs
      pure $ ResBasic $ LBoolean $ or args
    BuiltinOp LXor -> do
      args <- traverse runLispExprReturnBool sFunctionCallArgs
      pure $ ResBasic $ LBoolean $ foldl1' (/=) args
    BuiltinOp LEq -> do
      args <- traverse runLispExpr sFunctionCallArgs
      pure $ ResBasic $ LBoolean $ and $ zipWith (==) args (tail args)
    BuiltinOp LNeq -> do
      args <- traverse runLispExpr sFunctionCallArgs
      pure $ ResBasic $ LBoolean $ and $ zipWith (/=) args (tail args)
    BuiltinOp LGT -> do
      args <- traverse runLispExpr sFunctionCallArgs
      pure $ ResBasic $ LBoolean $ and $ zipWith (>) args (tail args)
    BuiltinOp LGTE -> do
      args <- traverse runLispExpr sFunctionCallArgs
      pure $ ResBasic $ LBoolean $ and $ zipWith (>=) args (tail args)
    BuiltinOp LLT -> do
      args <- traverse runLispExpr sFunctionCallArgs
      pure $ ResBasic $ LBoolean $ and $ zipWith (<) args (tail args)
    BuiltinOp LLTE -> do
      args <- traverse runLispExpr sFunctionCallArgs
      pure $ ResBasic $ LBoolean $ and $ zipWith (<=) args (tail args)
    BuiltinOp LDisplay -> do
      args <- traverse runLispExpr sFunctionCallArgs
      liftIO $ mapM_ (putStr . show) args
      pure ResEmpty
    BuiltinOp LDisplayLn -> do
      args <- traverse runLispExpr sFunctionCallArgs
      liftIO $ mapM_ print args
      pure ResEmpty
    BuiltinOp LError -> do
      args <- traverse runLispExpr sFunctionCallArgs
      throwError $ show $ head args
    BuiltinOp LExit -> do
      args <- traverse runLispExprReturnNumber sFunctionCallArgs
      liftIO $ exitWith $ ExitFailure $ floor $ head args
    BuiltinOp LLoad -> do
      undefined
    BuiltinOp LPrint -> do
      args <- traverse runLispExpr sFunctionCallArgs
      liftIO $ mapM_ (putStr . show) args
      pure ResEmpty
    BuiltinOp LAtomQ -> do
      let isAtom x = case x of
            ResBasic _ -> True
            _ -> False
      args <- traverse runLispExpr sFunctionCallArgs
      pure $ ResBasic $ LBoolean $ all isAtom args
    BuiltinOp LBooleanQ -> do
      let isBoolean x = case x of
            ResBasic (LBoolean _) -> True
            _ -> False
      args <- traverse runLispExpr sFunctionCallArgs
      pure $ ResBasic $ LBoolean $ all isBoolean args
    BuiltinOp LIntegerQ -> do
      let isInteger x = case x of
            ResBasic (LNumber n) -> n == fromInteger (floor n)
            _ -> False
      args <- traverse runLispExpr sFunctionCallArgs
      pure $ ResBasic $ LBoolean $ all isInteger args
    BuiltinOp LNumberQ -> do
      let isNumber x = case x of
            ResBasic (LNumber _) -> True
            _ -> False
      args <- traverse runLispExpr sFunctionCallArgs
      pure $ ResBasic $ LBoolean $ all isNumber args
    BuiltinOp LListQ -> do
      let isList x = case x of
            ResList _ -> True
            _ -> False
      args <- traverse runLispExpr sFunctionCallArgs
      pure $ ResBasic $ LBoolean $ all isList args
    BuiltinOp LProcedureQ -> do
      let isProcedure x = case x of
            ResFunction _ -> True
            ResLambda _ -> True
            _ -> False
      args <- traverse runLispExpr sFunctionCallArgs
      pure $ ResBasic $ LBoolean $ all isProcedure args
    BuiltinOp LStringQ -> do
      let isString x = case x of
            ResBasic (LString _) -> True
            _ -> False
      args <- traverse runLispExpr sFunctionCallArgs
      pure $ ResBasic $ LBoolean $ all isString args
    BuiltinOp LNullQ -> do
      let isNull x = case x of
            ResList [] -> True
            _ -> False
      args <- traverse runLispExpr sFunctionCallArgs
      pure $ ResBasic $ LBoolean $ all isNull args
    BuiltinOp LSymbolQ -> do
      let isSymbol x = case x of
            ResIdentifier _ -> True
            _ -> False
      args <- traverse runLispExpr sFunctionCallArgs
      pure $ ResBasic $ LBoolean $ all isSymbol args
    BuiltinOp LCar -> do
      args <- traverse runLispExprReturnList sFunctionCallArgs
      pure $ head $ head args
    BuiltinOp LCdr -> do
      args <- traverse runLispExprReturnList sFunctionCallArgs
      pure $ ResList $ tail $ head args
    BuiltinOp LCons -> do
      target <- runLispExpr $ head sFunctionCallArgs
      lst <- runLispExprReturnList $ sFunctionCallArgs !! 1
      pure $ ResList $ target : lst
    BuiltinOp LLength -> do
      lst <- runLispExprReturnList $ head sFunctionCallArgs
      pure $ ResBasic $ LNumber $ fromIntegral $ length lst
    BuiltinOp LList -> do
      args <- traverse runLispExpr sFunctionCallArgs
      pure $ ResList args
    BuiltinOp LAppend -> do
      args <- traverse runLispExprReturnList sFunctionCallArgs
      pure $ ResList $ concat args
    BuiltinOp LAbs -> do
      args <- traverse runLispExprReturnNumber sFunctionCallArgs
      pure $ ResBasic $ LNumber $ abs $ head args
    BuiltinOp LEqQ -> do
      args <- traverse runLispExprReturnNumber sFunctionCallArgs
      pure $ ResBasic $ LBoolean $ and $ zipWith (==) args (tail args)
    BuiltinOp LNot -> do
      args <- traverse runLispExprReturnBool sFunctionCallArgs
      pure $ ResBasic $ LBoolean $ not $ head args
    BuiltinOp LFormat -> do
      fmt <- runLispExprReturnString $ head sFunctionCallArgs
      args <- traverse runLispExpr $ tail sFunctionCallArgs
      pure $ ResBasic $ LString $ T.pack $ printf fmt args
    BuiltinOp LToNumber -> do
      args <- traverse runLispExprReturnString sFunctionCallArgs
      pure $ ResBasic $ LNumber $ read $ head args
    BuiltinOp LToString -> do
      args <- traverse runLispExpr sFunctionCallArgs
      pure $ ResBasic $ LString $ T.pack $ show $ head args
    BuiltinOp LToBoolean -> do
      args <- traverse runLispExpr sFunctionCallArgs
      pure $ ResBasic $ LBoolean $ case head args of
        ResBasic (LBoolean b) -> b
        ResBasic (LNumber n) -> n /= 0
        ResBasic (LString s) -> not $ T.null s
        ResList l -> not $ null l
        _ -> False
    BuiltinOp LNth -> do
      lst <- runLispExprReturnList $ head sFunctionCallArgs
      idx <- runLispExprReturnNumber $ sFunctionCallArgs !! 2
      pure $ lst !! floor idx
    BuiltinOp LRead -> do
      res <- liftIO readLn
      pure $ ResBasic $ LString $ T.pack res
    BuiltinOp LSqrt -> do
      args <- traverse runLispExprReturnNumber sFunctionCallArgs
      pure $ ResBasic $ LNumber $ sqrt $ head args
    UserDefined funcName -> do
      oldEnv <- gets functionCallStack
      funcEnv <- gets functions
      case funcEnv Map.!? funcName of
        Just SFunctionDef {..} -> do
          args <- traverse runLispExpr sFunctionCallArgs
          let params = sFunctionArgs
          let body = NE.toList sFunctionBody
          let newEnv = Map.fromList $ zip params args
          modify (\s -> s {functionCallStack = newEnv <> oldEnv})
          res <- traverse runLispExpr body
          modify (\s -> s {functionCallStack = oldEnv})
          pure $ last res
        Nothing -> do
          res <- runLispExpr $ SLIdentifierCall funcName
          case res of
            ResFunction f -> do
              let cons = case readMaybe (T.unpack $ getName $ sFunctionName f) of
                    Nothing -> UserDefined $ sFunctionName f
                    Just b -> BuiltinOp b
              runFunctionCall $ SFunctionCall cons sFunctionCallArgs Map.empty AnyType
            ResLambda l -> runLambdaCall $ SLambdaCall l sFunctionCallArgs Map.empty AnyType
            ResBasic b -> pure $ ResBasic b
            ResList l -> pure $ ResList l
            _ -> throwError $ "Undefined(SFunctionCall): " <> T.unpack (getName funcName)

runIdentifierCall :: SIdentifierCall -> Interpreter ResultType
runIdentifierCall SIdentifierCall {..} = do
  callStack <- gets functionCallStack
  identEnv <- gets identifiers
  case identEnv Map.!? sIdentifierCallName of
    Just SIdentifierDef {..} -> do
      let expr = NE.toList sIdentifierBody
      res <- traverse runLispExpr expr
      pure $ last res
    Nothing -> case callStack Map.!? sIdentifierCallName of
      Nothing -> throwError $ "Undefined(SIdentifierCall): " <> T.unpack (getName sIdentifierCallName)
      Just v -> pure v

runLambdaCall :: SLambdaCall -> Interpreter ResultType
runLambdaCall SLambdaCall {..} = do
  oldEnv <- gets functionCallStack
  let lambdaDef = sLambdaCall
  let params = sLambdaArgs lambdaDef
  args <- traverse runLispExpr sLambdaCallArgs
  let body = NE.toList $ sLambdaBody lambdaDef
  let newEnv = Map.fromList $ zip params args
  modify (\s -> s {functionCallStack = newEnv <> oldEnv})
  res <- traverse runLispExpr body
  modify (\s -> s {functionCallStack = oldEnv})
  pure $ last res

runIf :: SIf -> Interpreter ResultType
runIf SIf {..} = do
  cond <- runLispExprReturnBool sLispIf
  if cond then runLispExpr sLispThen else maybe (pure ResEmpty) runLispExpr sLispElse

runCond :: SCond -> Interpreter ResultType
runCond SCond {..} = do
  let runCondClause (cond, expr) = do
        condRes <- runLispExprReturnBool cond
        if condRes then runLispExpr expr else pure ResEmpty
  res <- foldlM (\acc x -> if acc == ResEmpty then runCondClause x else pure acc) ResEmpty sCondCases
  case sCondElse of
    Nothing -> pure res
    Just e -> if res == ResEmpty then runLispExpr e else pure res

runLispExpr :: SLispExpr -> Interpreter ResultType
runLispExpr = \case
  SLBasic t -> pure $ ResBasic t
  SLList l -> ResList <$> traverse runLispExpr l
  SLFunctionDef f -> do
    modify (\s -> s {functions = (\(k, v) -> Map.insert k v (functions s)) (buildFunctionDefPair f)})
    pure ResEmpty
  SLFunctionCall f -> runFunctionCall f
  SLIdentifierDef d -> do
    modify (\s -> s {identifiers = (\(k, v) -> Map.insert k v (identifiers s)) (buildIdentifierDefPair d)})
    pure ResEmpty
  SLIf i -> runIf i
  SLLambdaDef l -> pure $ ResLambda l
  SLLambdaCall l -> runLambdaCall l
  SLCond c -> runCond c
  SLIdentifierCall i -> do
    funcEnv <- gets functions
    identEnv <- gets identifiers
    stack <- gets functionCallStack

    case stack Map.!? i of
      Just v -> pure v
      Nothing ->
        case funcEnv Map.!? i of
          Just v -> pure $ ResFunction v
          Nothing ->
            case identEnv Map.!? i of
              Just SIdentifierDef {..} -> NE.last <$> traverse runLispExpr sIdentifierBody
              Nothing -> throwError $ "Undefined(SLispExpr): " <> T.unpack (getName i)

-- | Run @runLispExpr@ and expect its return value is @BooleanType@
runLispExprReturnBool :: SLispExpr -> Interpreter Bool
runLispExprReturnBool expr = do
  res <- runLispExpr expr
  case res of
    ResBasic (LBoolean b) -> pure b
    _ -> throwError "Expect bool value"

-- | Run @runLispExpr@ and expect its return value is @NumberType@
runLispExprReturnNumber :: SLispExpr -> Interpreter LispNumber
runLispExprReturnNumber expr = do
  res <- runLispExpr expr
  case res of
    ResBasic (LNumber n) -> pure n
    _ -> throwError "Expect number value"

-- | Run @runLispExpr@ and expect its return value is @ListType@
runLispExprReturnList :: SLispExpr -> Interpreter [ResultType]
runLispExprReturnList expr = do
  res <- runLispExpr expr
  case res of
    ResList l -> pure l
    _ -> throwError "Expect list value"

-- | Run @runLispExpr@ and expect its return value is @StringType@
runLispExprReturnString :: SLispExpr -> Interpreter String
runLispExprReturnString expr = do
  res <- runLispExpr expr
  case res of
    ResBasic (LString s) -> pure $ T.unpack s
    _ -> throwError "Expect string value"

buildFunctionDefPair :: SFunctionDef -> (IdentifierName, SFunctionDef)
buildFunctionDefPair f = (sFunctionName f, f)

buildIdentifierDefPair :: SIdentifierDef -> (IdentifierName, SIdentifierDef)
buildIdentifierDefPair d = (sIdentifierName d, d)
