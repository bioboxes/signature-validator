import Signature.Parsing
import Schema.Builder

import System.Environment (getArgs)
import System.Console.GetOpt

data Flag = Signature String | Schema String
  deriving Show

options :: [OptDescr Flag]
options =
    [ Option ['s'] ["signature"] (ReqArg Signature "SIGNATURE") "the biobox signature"
    , Option ['e'] ["schema"]    (ReqArg Schema    "SCHEMA")    "which schema type - input|output"
    ]

processArgs :: [String] -> IO ([Flag], [String])
processArgs argv =
  case getOpt Permute options argv of
     (o, n, [])   -> return  (o, n)
     (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: "

main = do
  args       <- getArgs
  (flags, _) <- processArgs args
  putStrLn $ unwords $ map show flags


  putStrLn build
