module DataTypes where

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

data Value = VInt Integer | VBool Bool | VString String | VFunc Env Type [Arg] Block | VTup [Value]
    deriving (Eq, Ord)

instance Show (Value) where
    show (VInt a) = show a
    show (VBool a) = show a
    show (VString a) = show a
    show (VFunc env t args block) = show "FUNCTION: ..."
    show (VTup a) = show a


data RetInfo = Return Value | RetEnv Env | RetBreak | RetContinue

initEnv :: Env
initEnv = M.empty

initStore :: Store
initStore = M.empty


type InterpreterM a = ReaderT Env (ExceptT String (StateT Store IO)) a

runInterM :: Env -> Store -> InterpreterM a -> IO (Either String a, Store)
runInterM env state ev = runStateT (runExceptT (runReaderT ev env)) state