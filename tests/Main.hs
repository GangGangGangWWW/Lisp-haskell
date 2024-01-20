{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import Data.Text qualified as T
import Language.Lisp.Parser
import Language.Lisp.Type
import System.Random
import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ

-- | Max number of spaces for input one side
mAX_SPACE_NUMBER_PER_SIDE :: Int
mAX_SPACE_NUMBER_PER_SIDE = 10

{-# HLINT ignore "Use camelCase" #-}

-- | Insert spaces randomly to input to test our parser.
insertSpacesR :: SourceCode -> IO SourceCode
insertSpacesR s = do
  lSize :: Int <- randomRIO (0, mAX_SPACE_NUMBER_PER_SIDE)
  rSize :: Int <- randomRIO (0, mAX_SPACE_NUMBER_PER_SIDE)
  pure $ T.replicate lSize " " <> s <> T.replicate rSize " "

-- | Escape the escape character to printable style
escape :: T.Text -> T.Text
escape s = foldr (uncurry T.replace) s pat
  where
    pat :: [(T.Text, T.Text)]
    pat =
      [ ("\a", "\\a"),
        ("\b", "\\b"),
        ("\r", "\\r"),
        ("\n", "\\n"),
        ("\t", "\\t"),
        ("\f", "\\f"),
        ("\v", "\\v")
      ]

-- | Test case for LispExpr parser that expect parse succeed and the result is equal to expected
runTestCase :: (Eq a, Show a) => LispParser a -> SourceCode -> a -> TestTree
runTestCase parser s expected = runTestCaseImpl parser s (Just expected)

-- | Test case for LispExpr parser that expect the parser runs failed
runTestCase' :: (Eq a, Show a) => LispParser a -> SourceCode -> TestTree
runTestCase' parser s = runTestCaseImpl parser s Nothing

runTestCaseImpl :: (Eq a, Show a) => LispParser a -> SourceCode -> Maybe a -> TestTree
runTestCaseImpl parser s expected =
  testCase (T.unpack $ escape s) $ do
    s' <- insertSpacesR s
    assertEqual (T.unpack $ escape s) expected (runLispHsParserMaybe parser s')

basicTypeParserTests :: TestTree
basicTypeParserTests =
  testGroup
    "basic type"
    [ test "0" (LNumber 0),
      test "1" (LNumber 1),
      test "-1" (LNumber (-1)),
      test "42" (LNumber 42),
      test "99999" (LNumber 99999),
      test "-99999" (LNumber (-99999)),
      test "+3" (LNumber 3),
      test "1.2" (LNumber 1.2),
      test "-1.2" (LNumber (-1.2)),
      test "+1.2" (LNumber 1.2),
      test "1.2e3" (LNumber 1200),
      test "1.2e-3" (LNumber 0.0012),
      test "1.2e+3" (LNumber 1200),
      test' "++3",
      test' "--3",
      test' "+-3",
      test' "-+3",
      test' "aaaa",
      test' "0.",
      test' ".0",
      test' "a0",
      test' "0a",
      -- String tests
      testStr [r|hello|] (LString "hello"),
      testStr [r|hello here|] (LString "hello here"),
      testStr [r|hello,here|] (LString "hello,here"),
      testStr [r||] (LString ""),
      testStr [r|\"|] (LString "\""),
      testStr [r|\"\"|] (LString "\"\""),
      testStr [r|a\nb|] (LString "a\nb"),
      testStr "\a\b\t\r\v\f./,;'" (LString "\a\b\t\r\v\f./,;'"),
      testStr' [r|"123""|],
      -- Boolean tests
      test "#t" (LBoolean True),
      test "#f" (LBoolean False),
      test' "#true"
    ]
  where
    test :: SourceCode -> LispBasicType -> TestTree
    test = runTestCase lispBasicTypeParser

    test' :: SourceCode -> TestTree
    test' = runTestCase' lispBasicTypeParser

    testStr :: SourceCode -> LispBasicType -> TestTree
    testStr = runTestCase lispBasicTypeParser . wrap

    testStr' :: SourceCode -> TestTree
    testStr' = runTestCase' lispBasicTypeParser . wrap

    wrap :: T.Text -> T.Text
    wrap s = "\"" <> s <> "\""

exprListParserTests :: TestTree
exprListParserTests =
  testGroup
    "list"
    [ test "'()" [],
      test "'(1)" [LBasic $ LNumber 1],
      test "'(1 2 3)" [LBasic $ LNumber 1, LBasic $ LNumber 2, LBasic $ LNumber 3],
      test [r|'(1 "a" #t)|] (LBasic <$> [LNumber 1, LString "a", LBoolean True]),
      test "'('())" [LList []],
      test "'('('()) '())" [LList [LList []], LList []]
    ]
  where
    test :: SourceCode -> [LispExpr] -> TestTree
    test = runTestCase exprListParser

functionDefParserTests' :: (Eq a, Show a) => LispParser a -> (FunctionDef -> a) -> TestTree
functionDefParserTests' parser wrapper =
  testGroup
    "function define"
    [ test "(define (x) 42)" (FunctionDef (IdentifierName "x") [] [LBasic $ LNumber 42]),
      test "(define(x)42)" (FunctionDef (IdentifierName "x") [] [LBasic $ LNumber 42])
    ]
  where
    test :: SourceCode -> FunctionDef -> TestTree
    test src = runTestCase parser src . wrapper

functionCallParserTests' :: (Eq a, Show a) => LispParser a -> (FunctionCall -> a) -> TestTree
functionCallParserTests' parser wrapper =
  testGroup
    "function call"
    [ test "(x)" (FunctionCall (UserDefined $ IdentifierName "x") []),
      test "(+ 2 3)" (FunctionCall (BuiltinOp LAdd) [LBasic $ LNumber 2, LBasic $ LNumber 3]),
      test "(+ 2 3 4)" (FunctionCall (BuiltinOp LAdd) [LBasic $ LNumber 2, LBasic $ LNumber 3, LBasic $ LNumber 4]),
      test "(* 2 0)" (FunctionCall (BuiltinOp LMul) [LBasic $ LNumber 2, LBasic $ LNumber 0])
    ]
  where
    test :: SourceCode -> FunctionCall -> TestTree
    test src = runTestCase parser src . wrapper

-- | Test case for LispExpr parser that expect parse succeed
lispExprParserTests :: TestTree
lispExprParserTests =
  testGroup
    "lisp expr"
    [ exprListParserTests,
      functionDefParserTests'
        lispExprParser
        LFunctionDef,
      functionCallParserTests' lispExprParser LFunctionCall
    ]

identifierNameParserTests :: TestTree
identifierNameParserTests =
  testGroup
    "identifier name"
    [ test "a" (IdentifierName "a"),
      test "a123" (IdentifierName "a123"),
      test "a-b" (IdentifierName "a-b"),
      test "a-b-c" (IdentifierName "a-b-c"),
      test "a-b-c-d" (IdentifierName "a-b-c-d"),
      test "a-b-c-d-e" (IdentifierName "a-b-c-d-e"),
      test "a-b-c-d-e-f" (IdentifierName "a-b-c-d-e-f"),
      test "a-b-c-d-e-f-g" (IdentifierName "a-b-c-d-e-f-g"),
      test "a-b-c-d-e-f-g-h" (IdentifierName "a-b-c-d-e-f-g-h"),
      test "a-b-c-d-e-f-g-h-i" (IdentifierName "a-b-c-d-e-f-g-h-i"),
      test "a-b-c-d-e-f-g-h-i-j" (IdentifierName "a-b-c-d-e-f-g-h-i-j"),
      test "a-b-c-d-e-f-g-h-i-j-k" (IdentifierName "a-b-c-d-e-f-g-h-i-j-k"),
      test "a-b-c-d-e-f-g-h-i-j-k-l" (IdentifierName "a-b-c-d-e-f-g-h-i-j-k-l"),
      test "a-b-c-d-e-f-g-h-i-j-k-l-m" (IdentifierName "a-b-c-d-e-f-g-h-i-j-k-l-m"),
      test "a-b-c-d-e-f-g-h-i-j-k-l-m-n" (IdentifierName "a-b-c-d-e-f-g-h-i-j-k-l-m-n"),
      test "a-b-c-d-e-f-g-h-i-j-k-l-m-n-o" (IdentifierName "a-b-c-d-e-f-g-h-i-j-k-l-m-n-o"),
      test "a-b-c-d-e-f-g-h-i-j-k-l-m-n-o-p" (IdentifierName "a-b-c-d-e-f-g-h-i-j-k-l-m-n-o-p"),
      test "a-b-c-d-e-f-g-h-i-j-k-l-m-n-o-p-q" (IdentifierName "a-b-c-d-e-f-g-h-i-j-k-l-m-n-o-p-q"),
      test "a?b" (IdentifierName "a?b"),
      test' "a!b",
      test' "a$b",
      test' "a%b",
      test "add" (IdentifierName "add"),
      test "is-even" (IdentifierName "is-even"),
      test "null?" (IdentifierName "null?"),
      test "is-nothing?" (IdentifierName "is-nothing?"),
      test "+=" (IdentifierName "+="),
      test "++" (IdentifierName "++")
    ]
  where
    test :: SourceCode -> IdentifierName -> TestTree
    test = runTestCase identifierNameParser

    test' :: SourceCode -> TestTree
    test' = runTestCase' identifierNameParser

functionCallNameParserTests :: TestTree
functionCallNameParserTests =
  testGroup
    "function call name"
    [ test "+" (BuiltinOp LAdd),
      test "-" (BuiltinOp LSub),
      test "*" (BuiltinOp LMul),
      test "/" (BuiltinOp LDiv),
      test "%" (BuiltinOp LRem),
      test "and" (BuiltinOp LAnd),
      test "or" (BuiltinOp LOr),
      test "xor" (BuiltinOp LXor),
      test "==" (BuiltinOp LEq),
      test "!=" (BuiltinOp LNeq),
      test ">" (BuiltinOp LGT),
      test ">=" (BuiltinOp LGTE),
      test "<" (BuiltinOp LLT),
      test "<=" (BuiltinOp LLTE),
      test "add" (UserDefined $ IdentifierName "add"),
      test "is-even" (UserDefined $ IdentifierName "is-even"),
      test "null?" (UserDefined $ IdentifierName "null?"),
      test "++" (UserDefined $ IdentifierName "++"),
      test "=" (UserDefined $ IdentifierName "=")
    ]
  where
    test :: SourceCode -> FunctionCallName -> TestTree
    test = runTestCase functionCallNameParser

functionDefParserTests :: TestTree
functionDefParserTests = functionDefParserTests' functionDefParser id

functionCallParserTests :: TestTree
functionCallParserTests = functionCallParserTests' functionCallParser id

simpleLispExprTests :: TestTree
simpleLispExprTests =
  testGroup
    "lisp hs parser"
    [ basicTypeParserTests,
      lispExprParserTests,
      identifierNameParserTests,
      functionCallNameParserTests,
      --
      functionDefParserTests,
      functionCallParserTests
      --
      -- lispProgramParserTests
    ]

main :: IO ()
main = defaultMain simpleLispExprTests
