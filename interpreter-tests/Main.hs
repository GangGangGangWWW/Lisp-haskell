module Main where

import Control.Monad (void)
import Control.Monad.Except
import Control.Monad.State
import Data.Either.Extra
import Data.List.Extra
import Data.Text qualified as T
import Language.Lisp.Interpreter (InterpreterError, runInterpreter)
import Language.Lisp.Parser
import Language.Lisp.Semantic.Type (ToSemantic (toSemantic), TypeError)
import System.Directory.Extra
import System.Environment (setEnv)
import System.FilePath
import System.IO
import System.IO.Extra (captureOutput)
import System.Time.Extra (sleep)
import Test.Tasty
import Test.Tasty.HUnit

data LispHsError
  = FromSemantic TypeError
  | FromInterpreter InterpreterError

runInterpreterWithFile' :: FilePath -> ExceptT LispHsError IO ()
runInterpreterWithFile' fp = do
  !content <- liftIO $ T.pack <$> readFile' fp
  let ast = fromRight' $ runLispHsParser lispProgramParser fp content
  sast <- withExceptT FromSemantic $ evalStateT (toSemantic ast) mempty
  void $ withExceptT FromInterpreter $ evalStateT (runInterpreter sast) mempty

runInterpreterWithFile :: FilePath -> IO (Either LispHsError ())
runInterpreterWithFile = runExceptT . runInterpreterWithFile'

run :: TestName -> TestTree
run !testName =
  testCase testName $ do
    -- Force the test to be run in a different second to avoid flakiness,
    -- otherwise the expected output will contains unexpected values.
    sleep 0.001
    !expected <- readFile' goldenFile
    (!actual, _) <- captureOutput $ void $ runInterpreterWithFile input
    actual @?= expected
  where
    !base = "interpreter-tests" </> "data" </> testName
    !input = base </> "input"
    !goldenFile = base </> "golden"

allTests :: IO [String]
allTests = filter (not . isPrefixOf ".") <$> listDirectory ("interpreter-tests" </> "data")

tests :: [TestName] -> TestTree
tests trees =
  testGroup "tests" $ map run trees

main :: IO ()
main = do
  setEnv "TASTY_NUM_THREADS" "1"
  allTests >>= defaultMain . tests