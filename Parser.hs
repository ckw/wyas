import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import System.IO
import Control.Applicative ((<$>))
import Data.List (intercalate)
import Control.Monad.Error
import Data.IORef

main = do
    args <- getArgs
    case length args of
        0 -> fix $ \loop -> do
                 inp <- readPrompt "LI$P>"
                 unless (inp `elem` ["q","quit","exit"])
                     $ evalAndPrint inp >> loop
        1 -> evalAndPrint $ head args
        _ -> putStrLn "too many args"

flushStr str = putStr str >> hFlush stdout

readPrompt prompt = flushStr prompt >> getLine

evalString expr = return $ extractValue $ trapError
                      (liftM show $ readExpr expr >>= eval)

evalAndPrint expr = evalString expr >>= putStrLn

symbol = oneOf "!#%&|*+-/?@^_~=><"

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

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

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

type IOThrowsError = ErrorT LispError IO

liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows action = runErrorT (trapError action) >>= return . extractValue

--eval :: LispVal -> ThrowsError LispVal
eval v@(String _) = return v
eval v@(Number _) = return v
eval v@(Bool _) = return v
eval (List [Atom "quote", v]) = return v
eval (List [Atom "if", pred, conseq, alt]) = do
    res <- eval pred
    case res of
        Bool True -> eval conseq
        Bool False -> eval alt
        x -> throwError $ TypeMismatch "bool" x
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
             , ("=", numBoolBinop (==))
             , ("<", numBoolBinop (<))
             , (">", numBoolBinop (>))
             , ("/=", numBoolBinop (/=))
             , (">=", numBoolBinop (>=))
             , ("<=", numBoolBinop (<=))
             , ("&&", boolBoolBinop (&&))
             , ("||", boolBoolBinop (||))
             , ("string=?",  strBoolBinop (==))
             , ("string<?",  strBoolBinop (<))
             , ("string>?",  strBoolBinop (>))
             , ("string<=?", strBoolBinop (<=))
             , ("string>=?", strBoolBinop (>=))
             , ("car", car)
             , ("cdr", cdr)
             , ("cons", cons)
             , ("eq?", eqv)
             , ("eqv?", eqv)
             ]

boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ head args
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum

boolBoolBinop = boolBinop unpackBool

strBoolBinop = boolBinop unpackStr

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


unpackBool (Bool b) = return b
unpackBool t = throwError $ TypeMismatch "boolean" t


unpackStr (String s) = return s
unpackStr t = throwError $ TypeMismatch "string" t


car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [_] y] = return y
cdr [DottedList (_ : xs) y] = return $ DottedList xs y
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs y] = return $ DottedList (x:xs) y
cons [x, y] = return $ DottedList [x] y
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] =
    eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2)
                              && (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                                 Left err -> False
                                 Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList
