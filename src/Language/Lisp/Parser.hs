{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Lisp.Parser where

import Control.Monad (void)
import Data.List.NonEmpty qualified as NE
import Data.String (IsString)
import Data.Text qualified as T
import Data.Void
import Language.Lisp.Type
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type LispParser = Parsec Void SourceCode

-- | Skip spaces and comments
skip :: LispParser ()
skip = L.space space1 (L.skipLineComment ";") empty

-- | @L.lexeme@ wrapper that skipping spaces and comments
lexeme :: LispParser a -> LispParser a
lexeme = L.lexeme skip

-- | @L.symbol@ wrapper that skipping spaes and comments
symbol :: T.Text -> LispParser T.Text
symbol = L.symbol skip

parens :: LispParser a -> LispParser a
parens = between (symbol "(") (symbol ")")

identifierSpecialSymbols :: [Char]
identifierSpecialSymbols = ['+', '-', '*', '/', '?', '>', '<', '=']

reservedKeywords :: (IsString s) => [s]
reservedKeywords = ["if", "define", "lambda", "cond", "else"]

isReservedExpr :: LispExpr -> Bool
-- isReservedExpr (LIdentifier ident) = T.unpack ident `elem` reservedKeywords
isReservedExpr _ = False

runLispHsParser :: LispParser a -> String -> SourceCode -> Either (ParseErrorBundle SourceCode Void) a
runLispHsParser = runParser . (skip *>)

runLispHsParserMaybe :: LispParser a -> SourceCode -> Maybe a
runLispHsParserMaybe = parseMaybe . (skip *>)

lispBasicTypeParser :: LispParser LispBasicType
lispBasicTypeParser =
  choice
    [ lNumberParser,
      lStringParser,
      lBooleanParser
    ]
  where
    lNumberParser :: LispParser LispBasicType
    lNumberParser = LNumber <$> lexeme (L.signed mempty (try L.float <|> L.decimal))

    lBooleanParser :: LispParser LispBasicType
    lBooleanParser =
      choice
        [ LBoolean True <$ symbol "#t",
          LBoolean False <$ symbol "#f"
        ]

    lStringParser :: LispParser LispBasicType
    lStringParser =
      LString . T.pack
        <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))

-- | Identifier name parser
identifierNameParser :: LispParser IdentifierName
identifierNameParser =
  lexeme
    ( some (alphaNumChar <|> oneOf identifierSpecialSymbols) >>= \s -> do
        if s `elem` reservedKeywords
          then fail $ "reserved: " <> s
          else pure $ IdentifierName $ T.pack s
    )

exprListParser :: LispParser [LispExpr]
exprListParser = lexeme (char '\'' *> parens (many lispExprParser))

functionDefParser :: LispParser FunctionDef
functionDefParser = parens $ do
  void $ symbol "define"
  (name, args) <- parens $ do
    name' <- lexeme identifierNameParser
    args' <- many identifierNameParser
    return (name', args')
  body <- NE.fromList <$> some lispExprParser
  return $ FunctionDef name args body

functionCallParser :: LispParser FunctionCall
functionCallParser = parens $ do
  name <- lexeme identifierNameParser
  args <- many lispExprParser
  return $ FunctionCall name args

identifierDefParser :: LispParser IdentifierDef
identifierDefParser = parens $ do
  void $ symbol "define"
  name <- lexeme identifierNameParser
  body <- NE.fromList <$> some lispExprParser
  return $ IdentifierDef name body

identifierCallParser :: LispParser IdentifierCall
identifierCallParser = parens $ do
  name <- lexeme identifierNameParser
  return $ IdentifierCall name

lambdaDefParser :: LispParser LambdaDef
lambdaDefParser = parens $ do
  void $ symbol "lambda"
  params <- parens $ many identifierNameParser
  body <- NE.fromList <$> some lispExprParser
  return $ LambdaDef params body

lambdaCallParser :: LispParser LambdaCall
lambdaCallParser = parens $ do
  lambda <- lexeme lambdaDefParser
  args <- many lispExprParser
  return $ LambdaCall lambda args

ifParser :: LispParser If
ifParser = parens $ do
  void $ symbol "if"
  cond <- lexeme lispExprParser
  thenStat <- lexeme lispExprParser
  ( try lispExprParser >>= \elseStat ->
      pure $ If cond thenStat (Just elseStat)
    )
    <|> pure (If cond thenStat Nothing)

condParser :: LispParser Cond
condParser = parens $ do
  void $ symbol "cond"
  stmts <- some (try $ parens ((,) <$> lexeme lispExprParser <*> lexeme lispExprParser))
  ( try elseParser >>= \condElse ->
      pure $ Cond (NE.fromList stmts) (Just condElse)
    )
    <|> pure (Cond (NE.fromList stmts) Nothing)
  where
    elseParser :: LispParser LispExpr
    elseParser = parens $ do
      void $ symbol "else"
      lexeme lispExprParser

lispExprParser :: LispParser LispExpr
lispExprParser =
  choice $
    map
      (lexeme . try)
      [ LBasic <$> lispBasicTypeParser,
        LList <$> exprListParser,
        LIdentifier <$> identifierNameParser,
        LFunctionDef <$> functionDefParser,
        LFunctionCall <$> functionCallParser,
        LIdentifierDef <$> identifierDefParser,
        LLambdaDef <$> lambdaDefParser,
        LLambdaCall <$> lambdaCallParser,
        LIf <$> ifParser,
        LCond <$> condParser
      ]

lispProgramParser :: LispParser LispProgram
lispProgramParser = group <$> many parser
  where
    parser :: LispParser (SevenOf FunctionDef FunctionCall IdentifierDef IdentifierCall LambdaCall If Cond)
    parser =
      (One <$> try functionDefParser)
        <|> (Two <$> try functionCallParser)
        <|> (Three <$> try identifierDefParser)
        <|> (Four <$> try identifierCallParser)
        <|> (Five <$> try lambdaCallParser)
        <|> (Six <$> try ifParser)
        <|> (Seven <$> condParser)

    group :: [SevenOf FunctionDef FunctionCall IdentifierDef IdentifierCall LambdaCall If Cond] -> LispProgram
    group = foldr f (LispProgram [] [] [] [] [] [] [])
      where
        f (One x) LispProgram {..} = LispProgram {functionDefs = x : functionDefs, ..}
        f (Two x) LispProgram {..} = LispProgram {functionCalls = x : functionCalls, ..}
        f (Three x) LispProgram {..} = LispProgram {identifierDefs = x : identifierDefs, ..}
        f (Four x) LispProgram {..} = LispProgram {identifierCalls = x : identifierCalls, ..}
        f (Five x) LispProgram {..} = LispProgram {lambdaCalls = x : lambdaCalls, ..}
        f (Six x) LispProgram {..} = LispProgram {ifs = x : ifs, ..}
        f (Seven x) LispProgram {..} = LispProgram {conds = x : conds, ..}

-- | Type wrapper for @lispProgramParser@ result type to make the compiler happy
data SevenOf a b c d e f g = One a | Two b | Three c | Four d | Five e | Six f | Seven g
  deriving (Eq, Show)