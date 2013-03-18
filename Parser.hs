import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

main = do
    args <- getArgs
    putStrLn $ readExpr $ head args


symbol = oneOf "!#%&|*+-/?@^_~"

readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"
