module CompilerJVM where

import Data.Map
import Control.Monad.State

import AbsInstant
import ErrM
import LexInstant
import ParInstant
import PrintInstant

type Loc = Int
type Env = Map Ident Loc
type Memory = StateT Env IO
-- Stack size and resulting code
type CompileRes = (Int, String)

compileProgram :: Program -> String -> IO String
compileProgram program name = do
	(mainContent, _) <- runStateT (translateProgram program) empty
	return (".class public " ++ name ++ "\n" ++
			".super java/lang/Object\n" ++
			".method public <init>()V\n" ++
			"  aload_0\n" ++
			"  invokespecial java/lang/Object/<init>()V\n" ++
			"  return\n" ++
			".end method\n" ++
			".method public static main([Ljava/lang/String;)V\n" ++ mainContent ++
			"return\n" ++
			".end method\n")

translateProgram :: Program -> Memory String
translateProgram (Prog stmts) = do
	results <- mapM translateStmt stmts
	let (stackSize, result) = Prelude.foldl (
			\(resSize, resCode) (size, code) -> (max resSize size, resCode ++ code)) 
		(0, "") results
	env <- get
	let localsSize = (size env) + 1
	return $ ".limit stack " ++ show stackSize ++ "\n.limit locals " ++ show localsSize ++ "\n" ++ result

translateStmt :: Stmt -> Memory CompileRes
translateStmt (SAss ident expr) = do
	(exprStack, exprCode) <- translateExp expr
	env <- get
	loc <- case Data.Map.lookup ident env of
		Just loc -> return loc
		Nothing -> do
			let newLoc = (Data.Map.size env) + 1
			let newEnv = insert ident newLoc env
			put newEnv
			return newLoc
	return (exprStack, exprCode ++ "istore " ++ (show loc) ++ "\n")
translateStmt (SExp expr) = do
	(stack, code) <- translateExp expr
	return (stack, code ++ "getstatic java/lang/System/out Ljava/io/PrintStream;\nswap\n" ++
			"invokevirtual java/io/PrintStream/println(I)V\n")

translateExp :: Exp -> Memory CompileRes
translateExp (ExpLit num) = return (1, "bipush " ++ (show num) ++ "\n")
translateExp (ExpVar ident) = do
	env <- get
	let loc = env ! ident
	return (1, "iload " ++ (show loc) ++ "\n")
translateExp (ExpAdd expr1 expr2) = translateBinaryOp expr1 "iadd" expr2
translateExp (ExpSub expr1 expr2) = translateBinaryOp expr1 "isub" expr2
translateExp (ExpMul expr1 expr2) = translateBinaryOp expr1 "imul" expr2
translateExp (ExpDiv expr1 expr2) = translateBinaryOp expr1 "idiv" expr2 

translateBinaryOp :: Exp -> String -> Exp -> Memory CompileRes
translateBinaryOp expr1 op expr2 = do
	(expr1Stack, expr1Code) <- translateExp expr1
	(expr2Stack, expr2Code) <- translateExp expr2
	if expr1Stack < expr2Stack then
		return (expr2Stack, expr2Code ++ expr1Code ++ "swap\n" ++ op ++ "\n")
	else
		return (max expr1Stack (expr2Stack + 1), expr1Code ++ expr2Code ++ op ++ "\n")
