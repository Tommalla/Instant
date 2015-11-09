module StaticChecker where

import AbsInstant
import Data.Set
import Control.Monad.Except
import Control.Monad.State
import System.IO (stderr, hPutStrLn)

type Env = Set Ident
type Res = StateT Env (ExceptT String IO)

staticCheck :: Program -> IO Bool
staticCheck program = do
	result <- runExceptT (runStateT (checkProgram program) empty)
	case result of
		Right _ -> return True
		Left errorMsg -> do
			hPutStrLn stderr $ "Static check failed: " ++ errorMsg
			return False

checkProgram :: Program -> Res ()
checkProgram (Prog stmts) = do
	mapM_ checkStmt stmts
	return ()

checkStmt :: Stmt -> Res ()
checkStmt (SAss ident expr) = do
	checkExp expr
	env <- get
	when (not . member ident $ env) (do put (insert ident env))
checkStmt (SExp expr) = do
	checkExp expr

checkExp :: Exp -> Res ()
checkExp (ExpLit _) = return ()
checkExp (ExpVar ident) = do
	env <- get
	if not . member ident $ env then do 
		error (show ident ++ " not defined.")
	else return ()
checkExp (ExpAdd expr1 expr2) = checkBinaryOp expr1 expr2
checkExp (ExpSub expr1 expr2) = checkBinaryOp expr1 expr2
checkExp (ExpMul expr1 expr2) = checkBinaryOp expr1 expr2
checkExp (ExpDiv expr1 expr2) = checkBinaryOp expr1 expr2

checkBinaryOp :: Exp -> Exp -> Res ()
checkBinaryOp expr1 expr2 = do
	checkExp expr1
	checkExp expr2
	return ()
