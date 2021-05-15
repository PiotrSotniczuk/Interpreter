module Helpers where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.IO.Class

import qualified Data.Map.Lazy as M

import AbsFlatte
import LexFlatte
import ParFlatte
import SkelFlatte
import ErrM
import PrintFlatte

type Loc = Int
type Env = M.Map Ident Loc
type Store = M.Map Loc Value

data Value = VInt Integer | VBool Bool | VString String | VFunc Env [Arg] Block | VNull
    deriving (Eq, Ord)


initEnv :: Env
initEnv = M.empty

initStore :: Store
initStore = M.empty


type InterpreterM a = ReaderT Env (ExceptT String (StateT Store IO)) a

runInterM :: Env -> Store -> InterpreterM a -> IO (Either String a, Store)
runInterM env state ev = runStateT (runExceptT (runReaderT ev env)) state


runInterpreter :: Program -> InterpreterM Integer
runInterpreter (ProgramDef pos dec) = do
    liftIO $ putStrLn $ show pos
    --env      <- declarations dec
    --VInt val <- local (const env) $ evalExpr $ EApp (Ident "main") []
    return 69