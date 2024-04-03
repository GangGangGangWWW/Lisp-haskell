{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import Data.List.NonEmpty qualified as NE
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
  testCase (trimName s) $ do
    s' <- insertSpacesR s
    assertEqual (T.unpack $ escape s) expected (runLispHsParserMaybe parser s')
  where
    trimName = T.unpack . T.strip . T.take 40 . escape

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

exprIdentifierParserTests :: TestTree
exprIdentifierParserTests =
  testGroup
    "identifier"
    [ test "word" (LIdentifier "word")
    ]
  where
    test :: SourceCode -> LispExpr -> TestTree
    test = runTestCase lispExprParser

functionDefParserTests' :: (Eq a, Show a) => LispParser a -> (FunctionDef -> a) -> TestTree
functionDefParserTests' parser wrapper =
  testGroup
    "function define"
    [ test "(define (x) 42)" (FunctionDef (IdentifierName "x") [] [LBasic $ LNumber 42]),
      test "(define(x)42)" (FunctionDef (IdentifierName "x") [] [LBasic $ LNumber 42]),
      test
        "(define (add a b) (+ a b))"
        ( FunctionDef
            (IdentifierName "add")
            [IdentifierName "a", IdentifierName "b"]
            [ LFunctionCall $
                FunctionCall
                  "+"
                  [LIdentifier $ IdentifierName "a", LIdentifier $ IdentifierName "b"]
            ]
        ),
      test
        "(define (f x) (* x (f x)))"
        ( FunctionDef
            (IdentifierName "f")
            [IdentifierName "x"]
            [ LFunctionCall $
                FunctionCall
                  "*"
                  [ LIdentifier "x",
                    LFunctionCall $
                      FunctionCall
                        (IdentifierName "f")
                        [LIdentifier $ IdentifierName "x"]
                  ]
            ]
        )
    ]
  where
    test :: SourceCode -> FunctionDef -> TestTree
    test src = runTestCase parser src . wrapper

identifierDefParserTests' :: (Eq a, Show a) => LispParser a -> (IdentifierDef -> a) -> TestTree
identifierDefParserTests' parser wrapper =
  testGroup
    "identifier definitions"
    [ test "(define x 42)" (IdentifierDef (IdentifierName "x") [LBasic $ LNumber 42]),
      test
        "(define f (define (g x) (+ x 1)))"
        ( IdentifierDef
            (IdentifierName "f")
            [ LFunctionDef $
                FunctionDef
                  (IdentifierName "g")
                  [IdentifierName "x"]
                  [LFunctionCall $ FunctionCall "+" [LIdentifier $ IdentifierName "x", LBasic $ LNumber 1]]
            ]
        )
    ]
  where
    test :: SourceCode -> IdentifierDef -> TestTree
    test src = runTestCase parser src . wrapper

functionCallParserTests' :: (Eq a, Show a) => LispParser a -> (FunctionCall -> a) -> TestTree
functionCallParserTests' parser wrapper =
  testGroup
    "function call"
    [ test "(x)" (FunctionCall (IdentifierName "x") []),
      test "(+ 2 3)" (FunctionCall "+" [LBasic $ LNumber 2, LBasic $ LNumber 3]),
      test "(+ 2 3 4)" (FunctionCall "+" [LBasic $ LNumber 2, LBasic $ LNumber 3, LBasic $ LNumber 4]),
      test "(f a b)" (FunctionCall (IdentifierName "f") [LIdentifier $ IdentifierName "a", LIdentifier $ IdentifierName "b"]),
      test "(> x 3)" (FunctionCall ">" [LIdentifier $ IdentifierName "x", LBasic $ LNumber 3]),
      test "(+ a b)" (FunctionCall "+" [LIdentifier $ IdentifierName "a", LIdentifier $ IdentifierName "b"]),
      test "(* 2 0)" (FunctionCall "*" [LBasic $ LNumber 2, LBasic $ LNumber 0]),
      test
        "(+ x 2 y)"
        ( FunctionCall
            "+"
            [LIdentifier $ IdentifierName "x", LBasic $ LNumber 2, LIdentifier $ IdentifierName "y"]
        ),
      test
        "(+ a b (* a b))"
        ( FunctionCall
            "+"
            [LIdentifier $ IdentifierName "a", LIdentifier $ IdentifierName "b", LFunctionCall $ FunctionCall "*" [LIdentifier $ IdentifierName "a", LIdentifier $ IdentifierName "b"]]
        ),
      test "(+ 1 (* 2 x))" (FunctionCall "+" [LBasic (LNumber 1.0), LFunctionCall (FunctionCall "*" [LBasic (LNumber 2.0), LIdentifier "x"])])
    ]
  where
    test :: SourceCode -> FunctionCall -> TestTree
    test src = runTestCase parser src . wrapper

lambdaDefParserTests' :: (Eq a, Show a) => LispParser a -> (LambdaDef -> a) -> TestTree
lambdaDefParserTests' parser wrapper =
  testGroup
    "lambda define"
    [ test "(lambda (x) (x))" (LambdaDef [IdentifierName "x"] [LFunctionCall (FunctionCall "x" [])]),
      test
        "(lambda (x) x)"
        (LambdaDef [IdentifierName "x"] [LIdentifier "x"]),
      test
        "(lambda (x y) (* x y))"
        (LambdaDef [IdentifierName "x", IdentifierName "y"] [LFunctionCall $ FunctionCall "*" [LIdentifier $ IdentifierName "x", LIdentifier $ IdentifierName "y"]]),
      test "(lambda (x) 3)" (LambdaDef [IdentifierName "x"] [LBasic $ LNumber 3]),
      test' "(lambda (x) 3"
    ]
  where
    test :: SourceCode -> LambdaDef -> TestTree
    test src = runTestCase parser src . wrapper

    test' :: SourceCode -> TestTree
    test' = runTestCase' parser

lambdaCallParserTests' :: (Eq a, Show a) => LispParser a -> (LambdaCall -> a) -> TestTree
lambdaCallParserTests' parser wrapper =
  testGroup
    "lambda call"
    [ test
        " ((lambda (x) (+ x 1)) 2)"
        ( LambdaCall
            ( LambdaDef
                [IdentifierName "x"]
                [LFunctionCall $ FunctionCall "+" [LIdentifier $ IdentifierName "x", LBasic $ LNumber 1]]
            )
            [LBasic $ LNumber 2]
        )
    ]
  where
    test :: SourceCode -> LambdaCall -> TestTree
    test src = runTestCase parser src . wrapper

ifParserTests' :: (Eq a, Show a) => LispParser a -> (If -> a) -> TestTree
ifParserTests' parser wrapper =
  testGroup
    "if"
    [ test
        "(if (> 2 3) 1 2)"
        ( If
            ( LFunctionCall $
                FunctionCall
                  ">"
                  [LBasic $ LNumber 2, LBasic $ LNumber 3]
            )
            (LBasic $ LNumber 1)
            (Just (LBasic $ LNumber 2))
        ),
      test
        [r|(if (odd x) "odd" "even")|]
        ( If
            ( LFunctionCall $
                FunctionCall
                  (IdentifierName "odd")
                  [LIdentifier $ IdentifierName "x"]
            )
            (LBasic $ LString "odd")
            (Just (LBasic $ LString "even"))
        ),
      test
        [r|(if (odd x) "odd")|]
        ( If
            ( LFunctionCall $
                FunctionCall
                  (IdentifierName "odd")
                  [LIdentifier $ IdentifierName "x"]
            )
            (LBasic $ LString "odd")
            Nothing
        ),
      test
        [r|(if (> x 2) "true" (+ x 2 y))|]
        ( If
            ( LFunctionCall $
                FunctionCall
                  ">"
                  [LIdentifier $ IdentifierName "x", LBasic $ LNumber 2]
            )
            (LBasic $ LString "true")
            ( Just $
                LFunctionCall $
                  FunctionCall
                    "+"
                    [LIdentifier $ IdentifierName "x", LBasic $ LNumber 2, LIdentifier $ IdentifierName "y"]
            )
        )
    ]
  where
    test :: SourceCode -> If -> TestTree
    test src = runTestCase parser src . wrapper

condParserTests' :: (Eq a, Show a) => LispParser a -> (Cond -> a) -> TestTree
condParserTests' parser wrapper =
  testGroup
    "cond"
    [ test
        "(cond ((> 2 3) (+ 2 3)))"
        ( Cond
            ( NE.fromList
                [ ( LFunctionCall $
                      FunctionCall
                        ">"
                        [LBasic $ LNumber 2, LBasic $ LNumber 3],
                    LFunctionCall $
                      FunctionCall
                        "+"
                        [LBasic $ LNumber 2, LBasic $ LNumber 3]
                  )
                ]
            )
            Nothing
        ),
      test
        "(cond ((> 2 3) 4))"
        ( Cond
            ( NE.fromList
                [ ( LFunctionCall $
                      FunctionCall
                        ">"
                        [LBasic $ LNumber 2, LBasic $ LNumber 3],
                    LBasic $ LNumber 4
                  )
                ]
            )
            Nothing
        ),
      test
        "(cond ((> 2 3) 1) ((< 4 5) 2))"
        ( Cond
            ( NE.fromList
                [ ( LFunctionCall $
                      FunctionCall
                        ">"
                        [LBasic $ LNumber 2, LBasic $ LNumber 3],
                    LBasic $ LNumber 1
                  ),
                  ( LFunctionCall $
                      FunctionCall
                        "<"
                        [LBasic $ LNumber 4, LBasic $ LNumber 5],
                    LBasic $ LNumber 2
                  )
                ]
            )
            Nothing
        ),
      test
        "(cond ((> 2 3) 1) ((< 4 5) 2) (else 3))"
        ( Cond
            ( NE.fromList
                [ ( LFunctionCall $
                      FunctionCall
                        ">"
                        [LBasic $ LNumber 2, LBasic $ LNumber 3],
                    LBasic $ LNumber 1
                  ),
                  ( LFunctionCall $
                      FunctionCall
                        "<"
                        [LBasic $ LNumber 4, LBasic $ LNumber 5],
                    LBasic $ LNumber 2
                  )
                ]
            )
            (Just $ LBasic $ LNumber 3)
        ),
      test' "(cond)"
    ]
  where
    test :: SourceCode -> Cond -> TestTree
    test src = runTestCase parser src . wrapper

    test' :: SourceCode -> TestTree
    test' = runTestCase' parser

-- | Test case for LispExpr parser that expect parse succeed
lispExprParserTests :: TestTree
lispExprParserTests =
  testGroup
    "lisp expr"
    [ exprListParserTests,
      exprIdentifierParserTests,
      functionDefParserTests'
        lispExprParser
        LFunctionDef,
      functionCallParserTests' lispExprParser LFunctionCall,
      identifierDefParserTests' lispExprParser LIdentifierDef,
      lambdaDefParserTests' lispExprParser LLambdaDef,
      lambdaCallParserTests' lispExprParser LLambdaCall,
      ifParserTests' lispExprParser LIf,
      condParserTests' lispExprParser LCond
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
      test "a%b" (IdentifierName "a%b"),
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

condParserTests :: TestTree
condParserTests = condParserTests' condParser id

ifParserTests :: TestTree
ifParserTests = ifParserTests' ifParser id

lambdaDefParserTests :: TestTree
lambdaDefParserTests = lambdaDefParserTests' lambdaDefParser id

lambdaCallParserTests :: TestTree
lambdaCallParserTests = lambdaCallParserTests' lambdaCallParser id

identifierDefParserTest :: TestTree
identifierDefParserTest = identifierDefParserTests' identifierDefParser id

functionDefParserTests :: TestTree
functionDefParserTests = functionDefParserTests' functionDefParser id

functionCallParserTests :: TestTree
functionCallParserTests = functionCallParserTests' functionCallParser id

lispProgramParserTests :: TestTree
lispProgramParserTests =
  testGroup
    "lisp program"
    [ test
        [r|
(+ 5 3 4) ;Value: 12

(- 9 1) ;Value: 8

(/ 6 2) ;Value: 3

(+ (* 2 4) (- 4 6)) ;Value: 6

(define a 3) ;Value: a

(define b (+ a 1)) ;Value: b

(+ a b (* a b)) ;Value: 19

(= a b) ;Value: #f

(if (and (> b a) (< b (* a b)))
    b
    a) ;Value: 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ;Value: 16

(+ 2 (if (> b a) b a)) ;Value: 6

(* (cond ((> a b) a)
         ((< a b) b)
         (else 1))
   (+ a 1)) ;Value: 16
|]
        ( LispProgram
            { functionDefs = [],
              functionCalls =
                [ FunctionCall
                    { functionCallName = "+",
                      functionCallArgs = [LBasic (LNumber 5.0), LBasic (LNumber 3.0), LBasic (LNumber 4.0)]
                    },
                  FunctionCall {functionCallName = "-", functionCallArgs = [LBasic (LNumber 9.0), LBasic (LNumber 1.0)]},
                  FunctionCall {functionCallName = "/", functionCallArgs = [LBasic (LNumber 6.0), LBasic (LNumber 2.0)]},
                  FunctionCall
                    { functionCallName = "+",
                      functionCallArgs =
                        [ LFunctionCall
                            ( FunctionCall
                                { functionCallName = "*",
                                  functionCallArgs = [LBasic (LNumber 2.0), LBasic (LNumber 4.0)]
                                }
                            ),
                          LFunctionCall (FunctionCall {functionCallName = "-", functionCallArgs = [LBasic (LNumber 4.0), LBasic (LNumber 6.0)]})
                        ]
                    },
                  FunctionCall
                    { functionCallName = "+",
                      functionCallArgs =
                        [ LIdentifier "a",
                          LIdentifier "b",
                          LFunctionCall (FunctionCall {functionCallName = "*", functionCallArgs = [LIdentifier "a", LIdentifier "b"]})
                        ]
                    },
                  FunctionCall {functionCallName = "=", functionCallArgs = [LIdentifier "a", LIdentifier "b"]},
                  FunctionCall
                    { functionCallName = "+",
                      functionCallArgs =
                        [ LBasic (LNumber 2.0),
                          LIf
                            ( If
                                { lispIf =
                                    LFunctionCall
                                      ( FunctionCall
                                          { functionCallName = ">",
                                            functionCallArgs = [LIdentifier "b", LIdentifier "a"]
                                          }
                                      ),
                                  lispThen = LIdentifier "b",
                                  lispElse = Just (LIdentifier "a")
                                }
                            )
                        ]
                    },
                  FunctionCall
                    { functionCallName = "*",
                      functionCallArgs =
                        [ LCond
                            ( Cond
                                { condCases =
                                    ( LFunctionCall
                                        ( FunctionCall
                                            { functionCallName = ">",
                                              functionCallArgs = [LIdentifier "a", LIdentifier "b"]
                                            }
                                        ),
                                      LIdentifier "a"
                                    )
                                      NE.:| [(LFunctionCall (FunctionCall {functionCallName = "<", functionCallArgs = [LIdentifier "a", LIdentifier "b"]}), LIdentifier "b")],
                                  condElse = Just (LBasic (LNumber 1.0))
                                }
                            ),
                          LFunctionCall
                            ( FunctionCall
                                { functionCallName = "+",
                                  functionCallArgs =
                                    [ LIdentifier "a",
                                      LBasic (LNumber 1.0)
                                    ]
                                }
                            )
                        ]
                    }
                ],
              identifierDefs =
                [ IdentifierDef
                    { identifierName = "a",
                      identifierBody = LBasic (LNumber 3.0) NE.:| []
                    },
                  IdentifierDef {identifierName = "b", identifierBody = LFunctionCall (FunctionCall {functionCallName = "+", functionCallArgs = [LIdentifier "a", LBasic (LNumber 1.0)]}) NE.:| []}
                ],
              identifierCalls = [],
              lambdaCalls = [],
              ifs = [If {lispIf = LFunctionCall (FunctionCall {functionCallName = "and", functionCallArgs = [LFunctionCall (FunctionCall {functionCallName = ">", functionCallArgs = [LIdentifier "b", LIdentifier "a"]}), LFunctionCall (FunctionCall {functionCallName = "<", functionCallArgs = [LIdentifier "b", LFunctionCall (FunctionCall {functionCallName = "*", functionCallArgs = [LIdentifier "a", LIdentifier "b"]})]})]}), lispThen = LIdentifier "b", lispElse = Just (LIdentifier "a")}],
              conds = [Cond {condCases = (LFunctionCall (FunctionCall {functionCallName = "=", functionCallArgs = [LIdentifier "a", LBasic (LNumber 4.0)]}), LBasic (LNumber 6.0)) NE.:| [(LFunctionCall (FunctionCall {functionCallName = "=", functionCallArgs = [LIdentifier "b", LBasic (LNumber 4.0)]}), LFunctionCall (FunctionCall {functionCallName = "+", functionCallArgs = [LBasic (LNumber 6.0), LBasic (LNumber 7.0), LIdentifier "a"]}))], condElse = Just (LBasic (LNumber 25.0))}]
            }
        ),
      test
        [r|
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))     ; compute p'
                   (+ (* 2 p q) (square q))    ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
|]
        ( LispProgram
            { functionDefs =
                [ FunctionDef
                    { functionName = "fib",
                      functionArgs = ["n"],
                      functionBody =
                        LFunctionCall
                          ( FunctionCall
                              { functionCallName = "fib-iter",
                                functionCallArgs = [LBasic (LNumber 1.0), LBasic (LNumber 0.0), LBasic (LNumber 0.0), LBasic (LNumber 1.0), LIdentifier "n"]
                              }
                          )
                          NE.:| []
                    },
                  FunctionDef
                    { functionName = "fib-iter",
                      functionArgs = ["a", "b", "p", "q", "count"],
                      functionBody =
                        LCond
                          ( Cond
                              { condCases =
                                  (LFunctionCall (FunctionCall {functionCallName = "=", functionCallArgs = [LIdentifier "count", LBasic (LNumber 0.0)]}), LIdentifier "b")
                                    NE.:| [ ( LFunctionCall (FunctionCall {functionCallName = "even?", functionCallArgs = [LIdentifier "count"]}),
                                              LFunctionCall (FunctionCall {functionCallName = "fib-iter", functionCallArgs = [LIdentifier "a", LIdentifier "b", LFunctionCall (FunctionCall {functionCallName = "+", functionCallArgs = [LFunctionCall (FunctionCall {functionCallName = "square", functionCallArgs = [LIdentifier "p"]}), LFunctionCall (FunctionCall {functionCallName = "square", functionCallArgs = [LIdentifier "q"]})]}), LFunctionCall (FunctionCall {functionCallName = "+", functionCallArgs = [LFunctionCall (FunctionCall {functionCallName = "*", functionCallArgs = [LBasic (LNumber 2.0), LIdentifier "p", LIdentifier "q"]}), LFunctionCall (FunctionCall {functionCallName = "square", functionCallArgs = [LIdentifier "q"]})]}), LFunctionCall (FunctionCall {functionCallName = "/", functionCallArgs = [LIdentifier "count", LBasic (LNumber 2.0)]})]})
                                            )
                                          ],
                                condElse = Just (LFunctionCall (FunctionCall {functionCallName = "fib-iter", functionCallArgs = [LFunctionCall (FunctionCall {functionCallName = "+", functionCallArgs = [LFunctionCall (FunctionCall {functionCallName = "*", functionCallArgs = [LIdentifier "b", LIdentifier "q"]}), LFunctionCall (FunctionCall {functionCallName = "*", functionCallArgs = [LIdentifier "a", LIdentifier "q"]}), LFunctionCall (FunctionCall {functionCallName = "*", functionCallArgs = [LIdentifier "a", LIdentifier "p"]})]}), LFunctionCall (FunctionCall {functionCallName = "+", functionCallArgs = [LFunctionCall (FunctionCall {functionCallName = "*", functionCallArgs = [LIdentifier "b", LIdentifier "p"]}), LFunctionCall (FunctionCall {functionCallName = "*", functionCallArgs = [LIdentifier "a", LIdentifier "q"]})]}), LIdentifier "p", LIdentifier "q", LFunctionCall (FunctionCall {functionCallName = "-", functionCallArgs = [LIdentifier "count", LBasic (LNumber 1.0)]})]}))
                              }
                          )
                          NE.:| []
                    }
                ],
              functionCalls = [],
              identifierDefs = [],
              identifierCalls = [],
              lambdaCalls = [],
              ifs = [],
              conds = []
            }
        )
    ]
  where
    test = runTestCase lispProgramParser

simpleLispExprTests :: TestTree
simpleLispExprTests =
  testGroup
    "lisp hs parser"
    [ basicTypeParserTests,
      lispExprParserTests,
      identifierNameParserTests,
      --
      functionDefParserTests,
      functionCallParserTests,
      identifierDefParserTest,
      lambdaDefParserTests,
      lambdaCallParserTests,
      ifParserTests,
      condParserTests,
      --
      lispProgramParserTests
    ]

main :: IO ()
main = defaultMain simpleLispExprTests
