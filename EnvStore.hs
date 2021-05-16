module EnvStore where

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

import DataTypes

getNewLoc :: Store -> Int
getNewLoc s = M.size s + 1

setValToLoc :: Loc -> Value -> InterpreterM ()
setValToLoc loc val = do
    new_store <- M.insert loc val <$> get
    put new_store

saveVarInEnv :: Type -> Ident -> InterpreterM Env
saveVarInEnv t id = do
    loc <- getNewLoc <$> get
    env <- asks (M.insert id loc)
    case t of
        Int -> do 
            setValToLoc loc (VInt 0)
            return env
        Str -> do
            setValToLoc loc (VString "")
            return env
        Bool -> do
            setValToLoc loc (VBool True)
            return env

getLocFromVar :: Ident -> InterpreterM Loc
getLocFromVar id = do
    loc <- asks (M.lookup id)
    case loc of
        Nothing   -> throwError $ "Error: variable " ++ show id ++ " is not declared"
        Just addr -> return addr

getValFromLoc :: Loc -> InterpreterM Value
getValFromLoc loc = do
    val <- M.lookup loc <$> get
    case val of
        Nothing    -> throwError "CRITICAL ERROR: This never should've happen"
        Just v     -> return v

getVarVal :: Ident -> InterpreterM Value
getVarVal id = do
    loc <- getLocFromVar id
    getValFromLoc loc

setValToVar :: Ident -> Value -> InterpreterM ()
setValToVar id val = do   
    act_type <- getType val
    last_val <- getVarVal id
    last_type <- getType last_val    
    loc <- getLocFromVar id
    if (act_type == last_type) then
        setValToLoc loc val
    else 
        throwError "TYPE ERROR: value does not match declared type"

getType :: Value -> InterpreterM Type
getType (VBool a) = return Bool
getType (VString a) = return Str
getType (VInt a) = return Int
getType (VFunc env t args block) = return t
