module Common where

import Data.List
import Data.List.Split
import System.Environment (getArgs)
import System.IO

import AbsInstant
import ErrM
import LexInstant
import ParInstant
import PrintInstant
import StaticChecker (staticCheck)

type CompileFunc = Program -> String -> IO String
type UnderlyingCompileFunc = String -> String -> String -> IO ()

compileProgram :: String -> CompileFunc -> UnderlyingCompileFunc -> IO ()
compileProgram inputFile compileImplem issueUnderlyingCompile = do
	code <- readFile inputFile
	let tokens = myLexer code
	case pProgram tokens of
		Bad errorStr -> do
			putStrLn ("Eror at parsing: " ++ errorStr)
			putStrLn ("Tokens: " ++ (show tokens))
		Ok program -> do
			staticCheckFine <- staticCheck program
			if staticCheckFine then do
				let pathSplit = splitOn "/" inputFile
				let className = head $ splitOn "." $ last $ pathSplit
				let path = intercalate "/" $ take (length pathSplit - 1) pathSplit
				let newPath = path ++ (if path == "" then "" else "/") ++ className
				res <- (compileImplem program className)
				issueUnderlyingCompile res newPath className
			else return ()

commonMain :: CompileFunc -> UnderlyingCompileFunc -> IO ()
commonMain compileImplem issueUnderlyingCompile = do
	inputFiles <- getArgs
	case inputFiles of
		[inputFile] -> compileProgram inputFile compileImplem issueUnderlyingCompile