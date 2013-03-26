import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Applicative ((<$>))
import Data.List (intercalate)
import Control.Monad.Error

main = do
          args <- getArgs
          evaluated <- return $ liftM show $ readExpr (head args) >>= eval
          putStrLn $ extractValue $ trapError evaluated

symbol = oneOf "!#%&|*+-/?@^_~"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ charOrEscape
                 char '"'
                 return $ String x

charOrEscape = isEscape <|> isChar

isChar = noneOf "\""

isEscape = do try $ char '\\'
              e <- oneOf "nrt\"\\"
              case e of
                '\"' -> return '\"'
                'n' -> return '\n'
                'r' -> return '\r'
                't' -> return '\t'
                '\\' -> return '\\'
                _    -> fail "invalid escape character"

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many $ letter <|> digit <|> symbol
               let atom = first : rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = (many1 digit) >>= (return . Number . read)

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where
    show v = case v of
        Atom s -> s
        List l -> "(" ++ (intercalate " " $ show <$> l) ++ ")"
        DottedList llv lv -> "("
                          ++ (intercalate " " $ show <$> llv)
                          ++  " . "
                          ++ (show lv)
                          ++ ")"
        Number n -> show n
        String s -> "\"" ++ s ++ "\""
        Bool b -> if b then "#t" else "#f"

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
    show (UnboundVar msg varname) = msg ++ ": " ++ varname
    show (BadSpecialForm msg form) = msg ++ ": " ++ show form
    show (NotFunction msg func) = msg ++ ": " ++ show func
    show (NumArgs expected found) = "Expected " ++ show expected
                                 ++ " args; found values "
                                 ++ (intercalate " " $ show <$> found)
    show (TypeMismatch expected found) = "Invalid type: expected "
                                      ++ expected ++ ", found " ++ show found
    show (Parser parseErr) = "Parse error at " ++ show parseErr

instance Error LispError where
    noMsg = Default "An error occurred"
    strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

--eval :: LispVal -> ThrowsError LispVal
eval v@(String _) = return v
eval v@(Number _) = return v
eval v@(Bool _) = return v
eval (List [Atom "quote", v]) = return v
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badform = throwError $ BadSpecialForm "Unrecognized special form" badform

--apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction
                            "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives = [ ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop div)
             , ("mod", numericBinop mod)
             , ("quotient", numericBinop quot)
             , ("remainder", numericBinop rem)
             , ("boolean?", isBool)
             , ("string?", isString)
             , ("number?", isNumber)
             , ("list?", isList)
             ]

isBool (Bool _ : xs) = return $ Bool True
isBool _ = return $ Bool False

isString (String _ : xs) = return $ Bool True
isString _ = return $ Bool False

isList (List _ : xs) = return $  Bool True
isList (DottedList _ _ : xs) = return $ Bool True
isList _ = return $ Bool False

isNumber (Number _ : xs) = return $ Bool True
isNumber _ = return $ Bool False

numericBinop op sv@[_] = throwError $ NumArgs 2 sv
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum (Number n) = return n
unpackNum t = throwError $ TypeMismatch "number" t
