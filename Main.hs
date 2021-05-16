-- Program to test parser, automatically generated by BNF Converter.

module Main where

import Prelude
  ( ($)
  , Either(..)
  , Int, (>)
  , String, (++), unlines
  , Show, show
  , IO, (>>), (>>=), mapM_, putStrLn
  , FilePath
  , getContents, readFile
  )
import System.Environment ( getArgs )
import System.Exit        ( exitFailure, exitSuccess )
import Control.Monad      ( when )
import System.IO ( stdin, stderr, hGetContents, hPutStrLn )

import AbsFlatte   ()
import LexFlatte   ( Token )
import ParFlatte   ( pProgram, myLexer )
import PrintFlatte ( Print, printTree )
import SkelFlatte  ()

import DataTypes     (initEnv, initStore, runInterM)
import Helpers        (runInterpreter)

type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

runFile :: FilePath -> IO ()
runFile f = putStrLn f >> readFile f >>= run

run :: String -> IO ()
run s =
  case pProgram ts of
    Left err -> do
      putStrLn "\nParse              Failed...\n"
      putStrLn "Tokens:"
      putStrLn $ show ts
      putStrLn err
      exitFailure
    Right tree -> do
      putStrLn "\nParse Successful!"
      --showTree tree
      (val, store) <- runInterM initEnv initStore (runInterpreter tree)       
      case val of
            Left e  -> hPutStrLn stderr e
            Right code -> hPutStrLn stderr $ "Exit code: " ++ show code
      exitSuccess
  where
  ts = myLexer s

showTree :: (Show a, Print a) => a -> IO ()
showTree tree = do
  putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrLn $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> getContents >>= run
    "-s":fs    -> mapM_ (runFile) fs
    fs         -> mapM_ (runFile) fs
