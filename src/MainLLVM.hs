import System.Process

import Common (commonMain)
import CompilerLLVM (compileProgram)

issueUnderlyingCompile :: String -> String -> String -> IO ()
issueUnderlyingCompile code path _ = do
	putStrLn $ "Writing to: " ++ (path ++ ".ll")
	writeFile (path ++ ".ll") $ code
	system ("llvm-as " ++ path ++ ".ll")
	return ()	

main :: IO ()
main = commonMain compileProgram issueUnderlyingCompile