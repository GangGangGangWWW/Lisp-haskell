{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (void, when)
import Control.Monad.Except (runExceptT)
import Control.Monad.State
import Data.Text qualified as T
import Language.Lisp.Interpreter
import Language.Lisp.Parser
import Language.Lisp.Semantic.Analyser
import Language.Lisp.Semantic.Type
import Options.Applicative

data Arguments = Arguments
  { -- | The input file
    input :: FilePath,
    -- | Print the AST
    printAST :: Bool,
    -- | Print the SAST
    printSAST :: Bool,
    -- | Enable semantic checking
    semanticChecking :: Bool
  }
  deriving (Show, Eq)

cliParser :: Parser Arguments
cliParser =
  Arguments
    <$> strOption -- this should also support @./lisp-hs a.txt@
      ( long "input"
          <> short 'i'
          <> metavar "FILE"
          <> help "The input file"
      )
    <*> switch
      ( long "print-ast"
          <> short 'a'
          <> help "Print the AST"
      )
    <*> switch
      ( long "print-sast"
          <> short 's'
          <> help "Print the SAST"
      )
    <*> switch
      ( long "semantic-checking"
          <> short 'c'
          <> help "Enable semantic checking"
      )

main :: IO ()
main = do
  args@Arguments {..} <- execParser $ info (cliParser <**> helper) fullDesc
  print args
  content <- T.pack <$> readFile input
  case runLispHsParser lispProgramParser input content of
    Left e -> print e
    Right ast -> do
      when printAST $ print ast
      res <- runExceptT $ evalStateT (toSemantic ast) mempty
      case res of
        Left e -> print e
        Right sast -> do
          when printSAST $ print sast
          when semanticChecking $ void $ runExceptT $ evalStateT (runSemanticChecking sast) mempty
          intRes <- runExceptT $ evalStateT (runInterpreter sast) mempty
          case intRes of
            Left e -> print e
            Right _ -> pure ()
      pure ()