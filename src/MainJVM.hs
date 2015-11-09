import Data.List
import Data.List.Split
import System.IO (stdin, hGetContents)
import System.Environment (getArgs)
import System.Process

import AbsInstant
import CompilerJVM (compileProgram)
import ErrM
import LexInstant
import ParInstant
import PrintInstant
import StaticChecker (staticCheck)


compileProgram :: String -> IO ()
compileProgram inputFile = do
	code <- readFile inputFile
	let tokens = myLexer code
	case pProgram tokens of
		Bad errorStr -> do
			putStrLn "ERROR TODO"
		Ok program -> do
			staticCheckFine <- staticCheck program
			if staticCheckFine then do
				let pathSplit = splitOn "/" inputFile
				let className = head $ splitOn "." $ last $ pathSplit
				let path = intercalate "/" $ take (length pathSplit - 1) pathSplit
				let newPath = path ++ (if path == "" then "" else "/") ++ className
				res <- (CompilerJVM.compileProgram program className)
				putStrLn $ "Writing to: " ++ (newPath ++ ".j")
				writeFile (newPath ++ ".j") $ res
				system $ ("java -jar lib/jasmin.jar " ++ newPath ++ ".j && mv " ++ className ++ ".class " ++ 
						newPath ++ ".class")
				return ()
			else return ()


main :: IO ()
main = do
	inputFiles <- getArgs
	case inputFiles of
		[inputFile] -> Main.compileProgram inputFile