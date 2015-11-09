-- Tomasz Zakrzewski, tz336079, 2015
module CompilerLLVM where

import Control.Monad.State
import Data.Set
import Data.List.Split

import AbsInstant
import ErrM
import LexInstant
import ParInstant
import PrintInstant

type Env = Set String
-- Next loc in register and env:
type Mem = (Int, Env)
type Memory = StateT Mem IO

compileProgram :: Program -> String -> IO String
compileProgram program name = do
	(mainContent, _) <- runStateT (translateProgram program) (0, empty)
	return ("@.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1\n" ++
			"declare i32 @printf(i8*, ...) #1\n" ++ 
			"define i32 @main() #0 {\n" ++ mainContent ++
			"ret i32 0\n}")

translateProgram :: Program -> Memory String
translateProgram (Prog stmts) = do
	results <- mapM translateStmt stmts
	return $ concat results

translateStmt :: Stmt -> Memory String
translateStmt (SAss (Ident ident) expr) = do
	(exprRes, exprCode) <- translateExp expr
	(reg, env) <- get
	allocate <- (if (member ident $ env) then (return "")  else (
			do 
			put (reg, insert ident env)
			return $ "%" ++ ident ++ " = alloca i32\n"))
	return (exprCode ++ allocate ++
			"store i32 " ++ exprRes ++  ", i32* %" ++ ident ++ "\n")
translateStmt (SExp expr) = do
	(exprRes, exprCode) <- translateExp expr
	(reg, env) <- get
	put (reg + 1, env)
	return (exprCode ++ "call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str, i32 0, i32 0), i32"++
			" " ++ exprRes ++ ")\n")

translateExp :: Exp -> Memory (String, String)
translateExp (ExpLit num) = return (show num, "")
translateExp (ExpVar (Ident ident)) = do
	(reg, env) <- get
	let newReg = reg + 1
	put (newReg, env)
	return ("%" ++ (show newReg), "%" ++ (show newReg) ++ " = load i32* %" ++ ident ++ "\n")
translateExp (ExpAdd expr1 expr2) = translateBinaryOp expr1 "add" expr2
translateExp (ExpSub expr1 expr2) = translateBinaryOp expr1 "sub" expr2
translateExp (ExpMul expr1 expr2) = translateBinaryOp expr1 "mul" expr2
translateExp (ExpDiv expr1 expr2) = translateBinaryOp expr1 "sdiv" expr2

translateBinaryOp :: Exp -> String -> Exp -> Memory (String, String)
translateBinaryOp expr1 op expr2 = do
	(expr1Res, expr1Code) <- translateExp expr1
	(expr2Res, expr2Code) <- translateExp expr2
	(reg, env) <- get
	let newReg = reg + 1
	put (newReg, env)
	return ("%" ++ (show newReg), expr1Code ++ expr2Code ++ "%" ++ (show newReg) ++ " = " ++ op ++ " i32 " ++ 
			expr1Res ++ ", " ++ expr2Res ++ "\n")
