-- Tomasz Zakrzewski, tz336079, 2015
import System.Process

import Common (commonMain)
import CompilerJVM (compileProgram)

issueUnderlyingCompile :: String -> String -> String -> IO ()
issueUnderlyingCompile code path className = do
	putStrLn $ "Writing to: " ++ (path ++ ".j")
	writeFile (path ++ ".j") $ code
	system $ ("java -jar lib/jasmin.jar " ++ path ++ ".j && mv " ++ className ++ ".class " ++ path ++ ".class")
	return ()

main :: IO ()
main = commonMain compileProgram issueUnderlyingCompile