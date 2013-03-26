import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Applicative ((<$>))
import Data.List (intercalate)

main = do
    getArgs >>= print . eval . readExpr . head

symbol = oneOf "!#%&|*+-/?@^_~"

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

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


eval :: LispVal -> LispVal
eval v@(String _) = v
eval v@(Number _) = v
eval v@(Bool _) = v
eval (List [Atom "quote", v]) = v
eval (List (Atom func : args)) = apply func $ eval <$> args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

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

isBool (Bool _ : xs) = Bool True
isBool _ = Bool False

isString (String _ : xs) = Bool True
isString _ = Bool False

isList (List _ : xs) = Bool True
isList (DottedList _ _ : xs) = Bool True
isList _ = Bool False

isNumber (Number _ : xs) = Bool True
isNumber _ = Bool False

numericBinop op params = Number $ foldl1 op $ unpackNum <$> params

unpackNum (Number n) = n
unpackNum _ = 0
