{-# LANGUAGE OverloadedStrings #-}

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
import Text.Read (readMaybe)

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
  name <- lexeme functionCallNameParser
  args <- many lispExprParser
  return $ FunctionCall name args

lispExprParser :: LispParser LispExpr
lispExprParser =
  choice $
    map
      (lexeme . try)
      [ LBasic <$> lispBasicTypeParser,
        LList <$> exprListParser,
        LFunctionDef <$> functionDefParser,
        LFunctionCall <$> functionCallParser
      ]

functionCallNameParser :: LispParser FunctionCallName
functionCallNameParser =
  choice
    [ BuiltinOp <$> try lispBuiltinOpParser,
      UserDefined <$> identifierNameParser
    ]
  where
    lispBuiltinOpParser :: LispParser LispBuiltinOp
    lispBuiltinOpParser = do
      op <-
        choice $
          map
            (try . symbol)
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
              ">=",
              "<=",
              ">",
              "<"
            ]
      let res = readMaybe (T.unpack op)
      case res of
        Just op' -> return op'
        Nothing -> fail "Unknown builtin operator" -- This should never happen since we are using `choice` combinator
