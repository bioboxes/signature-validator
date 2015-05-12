import Signature.Parsing
import Signature.Types

import qualified Schema.Builder as Schema

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


selectSignature :: [Flag] -> Either String SigObj
selectSignature [(Signature sig), (Schema sch)] = f sch
  where f "input"  = fmap fst $ parseSignature sig
        f "output" = fmap snd $ parseSignature sig
        f x        = Left("Error: unknown schema type \"" ++ x ++ "\"")


finish :: Either String String -> IO()
finish = either (f stderr $ ExitFailure 1) (f stdout ExitSuccess)
  where f handle code output = do
                               hPutStrLn handle output
                               exitWith code


main = do
  args <- getArgs
  finish $ processArgs args >>= selectSignature >>= Schema.build
