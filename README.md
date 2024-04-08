# Lisp-hs

A Lisp interpreter implemented using Haskell language

## Use instructions
-  cabal run lisp-hs -- -i input.scm -a -s -c, in this common line input.scm is a source code file which lisp code wich supports you test.
   And -a -s -c are the switches for pring AST, SAST and control semanticChecking.
-  cabal run test all if you need test specific test. Please, follow "cabal test lisp-hs-interpreter -test" or "cabal test lisp-hs-test".

## Parser

- [ ] basic parser
- [ ] parser tests
- [ ] golden tests

### keywords parser

#### Core Interpreter

- [ ] `display` :: a -> Unit
- [ ] `displayln` :: a -> Unit
- [ ] `error` :: a -> Unit
- [ ] `exit` :: Number -> Unit
- [ ] `load` :: String -> Unit
- [ ] `print` :: a -> Unit

#### Type Checking

- [ ] `atom?` :: a -> Boolean
- [ ] `boolean?` :: a -> Boolean
- [ ] `integer?` :: a -> Boolean
- [ ] `number?` :: a -> Boolean
- [ ] `list?` :: a -> Boolean
- [ ] `procedure?` :: a -> Boolean
- [ ] `string?` :: a -> Boolean
- [ ] `null?` :: a -> Boolean
- [ ] `symbol?` :: a -> Boolean

#### List Manipulation

- [ ] `car` :: List -> a
- [ ] `cdr` :: List -> List
- [ ] `cons` :: a -> List -> List
- [ ] `length` :: List -> Number
- [ ] `list` :: List -> List -> List List
- [ ] `append` :: List -> ... -> List

#### Arithmetic Operations

- [ ] `abs` :: Number -> Number
- [ ] `+` :: Number -> ... -> Number
- [ ] `-` :: Number -> ... -> Number
- [ ] `*` :: Number -> ... -> Number
- [ ] `/` :: Number -> ... -> Number
- [ ] `%` :: Number -> ... -> Number
- [ ] `and` :: Number -> ... -> Number
- [ ] `or` :: Number -> ... -> Number
- [ ] `xor` :: Number -> ... -> Number
- [ ] `==` :: Number -> ... -> Number
- [ ] `!=` :: Number -> ... -> Number
- [ ] `>` :: Number -> ... -> Number
- [ ] `<` :: Number -> ... -> Number
- [ ] `>=` :: Number -> ... -> Number
- [ ] `<=` :: Number -> ... -> Number

#### Boolean Operations

- [ ] `eq?` :: a -> a -> a
- [ ] `not` :: a -> Boolean

#### String Manipulation

- [ ] `format` :: String -> ... -> String
- [ ] `to-number` :: a -> Number
- [ ] `to-string` :: a -> String
- [ ] `to-list` :: a -> List
- [ ] `read` :: String

### Tests

- [ ] keywords parser tests

> Reference: https://cs61a.org/articles/scheme-builtins/

## Semantic analyser

### Type inference

Inference symbols type as much as possible to support type checking, see paraer above for builtin functions' type.

### checking rules

#### Type checking

- [ ] `car` and `cdr` only works for lists.
- [ ] `exit` should call with a number
- [ ] `cons` accepts an item and a list
- [ ] `length` only works for lists
- [ ] `list` only accept lists as arguments
- [ ] Arithmetic Operations only works for numbers
- [ ] `format` only works for strings
- [ ] Check `if` and `lambda` conditions return boolean

#### variable scope checking

- [ ] Check every symbol in the function define has defined before using
- [ ] Check every symbol in the function call has defined before using
- [ ] Check every symbol in the identifier define has defined before using
- [ ] Check every symbol in the identifier call has defined before using
- [ ] Check every symbol in the lambda call has defined before using
- [ ] Check every symbol in the if has defined before using
- [ ] Check every symbol in the cond has defined before using

#### function calling checking

- [ ] Check the function define has exact argument number with their callings
- [ ] Check the lambda define has exact argument number with their callings
- [ ] Check if the function call have inappropriate type arguments
- [ ] Check if the lambda call have inappropriate type arguments

#### Other checking

- [ ] Can not load itself
- [ ] Can not load cyclically
- [ ] Duplicate Definition checking

### constant folding early

Fold recursively

- [ ] Replace mathmatic calculation with constant numbers
- [ ] Replace list operation with constant lists
- [ ] Calculate result for builtin functions if possible

### functions tags

To determine if a function/expression can be optimized during the semantic analysis, we need to attach a tag for every symbol in our AST to describe if this symbol is a constant value or can be reduced to a constant value. The symbol indicate every visible items in our AST.

- [ ] Constant tag
- [ ] Dynamic tag

## Interpreter

TBD

### Result cache

For function `(define (f a b) (+ a b))`, if `f` is a constant function, we can store `(f 2 3)` as its result is `5` to prevent redundant calculating.

- [ ] Cache result for constant function calls
- [ ] Cache result for constant identidier calls
- [ ] Cache result for constant lambda calls

## Optparser

- [ ] Enable/Disable constant folding
- [ ] Output AST
- [ ] Output Semantic AST
- [ ] Semantic checking(Type checking)

## Error

- [ ] Parse error
- [ ] Semantic error
- [ ] User raised error