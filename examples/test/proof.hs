import System.Process
import System.Exit

main :: IO ExitCode
main = check =<< system "stack exec -- liquid src/**.hs"

check :: ExitCode -> IO ExitCode
check ExitSuccess = return ExitSuccess
check (ExitFailure n) = error ("Faild in proof with " ++ show n ++ ".")
