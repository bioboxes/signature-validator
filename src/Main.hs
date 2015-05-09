import Signature.Parsing
import Schema.Builder

import System.Environment (getArgs)
import System.Console.GetOpt
import System.Exit
import System.IO

data Flag = Signature String | Schema String
  deriving Show

options :: [OptDescr Flag]
options =
    [ Option ['s'] ["signature"] (ReqArg Signature "SIGNATURE") "the biobox signature"
    , Option ['e'] ["schema"]    (ReqArg Schema    "SCHEMA")    "which schema type - input|output"
    ]

processArgs :: [String] -> Either String [Flag]
processArgs argv =
  case getOpt Permute options argv of
     (flags, _, [])   -> Right flags
     (_, _, errors)   -> Left (concat errors ++ usageInfo header options)
  where header = "Usage: "

evaluateInputs :: Either String [Flag] -> IO()
evaluateInputs (Right flags) = finish (build, stdout, ExitSuccess)
evaluateInputs (Left msg)    = finish (msg,   stderr, ExitFailure 1)

finish (output, handle, code) = do
  hPutStrLn handle output
  exitWith code

main = do
  args <- getArgs
  evaluateInputs $ processArgs args
