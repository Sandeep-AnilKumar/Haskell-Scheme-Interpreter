-- importing Parsec without spaces, as we will be using it later.
import Text.ParserCombinators.Parsec hiding(spaces)
import Control.Monad
import System.Environment
-- importing numeric method implementations, alomg with Ratio and Complex.
import Numeric
import Data.Ratio
import Data.Complex
-- built-in haskell's error function.
import Control.Monad.Error
-- for using vectors
import Data.Array

--Any instance that is type of "show", can be converted to string. We are extending the same for LispVal, by making it a member of "Show".
instance Show LispVal where show = showVal

-- Making LispError a member of show.
instance Show LispError where show = showError

-- making LispError a member of Error.
instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

-- Partially applying LispError on Either, so that we can use ThrowsError on any datatype.
type ThrowsError = Either LispError

-- Converting all our errors to String, and extracting value from it. The result of typeError is again an Either action, which will always have
-- valid right data.
trapError action = catchError action (return . show)

-- extracting the right value, so that it can be passed to other monad. There is no representation for left because, extractValue is called
-- only for catchError of Either monad, which will be evaluated when right construct of Either is evaluated.
extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- This will be our data item, consisting of Atom, List, and so on.
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
	     | Character Char	
	     | Ratio Rational
	     | Float Double
	     | Complex (Complex Double)
             | Vector (Array Int LispVal)

-- data type to represent different types of data.
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

-- Symbol takes a Parser and returns a char.
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

-- This will skip amny spaces infront of the input
spaces :: Parser ()
spaces = skipMany1 space

-- Implementation for escape characters like \t, \n...
escapedChars :: Parser Char
escapedChars = do char '\\' 
                  x <- oneOf "\\\"nrt" 
                  return $ case x of 
                    '\\' -> x
                    '"'  -> x
                    'n'  -> '\n'
                    'r'  -> '\r'
                    't'  -> '\t'

-- printing the string representation of the various LispValues. Each clause of the definition matches the definition in the right side.
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

--defining all the error types with their constructors.
showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

-- In Haskell "words" splits words and puts them into list. just like String.split("\\s+"), splits spaces in a string.
-- Unwords, takes a list of values and combines them or joins them into one string. Contrary to "words" defintion. 
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- This will take input as a LispVal and return a Double
toDouble :: LispVal -> Double
toDouble(Float f) = realToFrac f
toDouble(Number n) = fromIntegral n

-- Implemntation for Complex numbers like 2+i3, this will call toDouble if either x or y is double like 2.3 + i4.5
parseComplex :: Parser LispVal
parseComplex = do x <- (try parseFloat <|> parseDecimal2)
                  char '+' 
                  y <- (try parseFloat <|> parseDecimal2)
                  char 'i' 
                  return $ Complex (toDouble x :+ toDouble y)

-- implementation for character
parseCharacter :: Parser LispVal
parseCharacter = do
 try $ string "#\\"
 value <- try (string "newline" <|> string "space") 
         <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
 return $ Character $ case value of
    "space" -> ' '
    "newline" -> '\n'
    otherwise -> (value !! 0)

-- #t is for true, #f for false.
parseBool :: Parser LispVal
parseBool = do
    char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

-- A string is a combination of quotation marks, followed by nonquote characters and ending by a closing quote.
parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ escapedChars <|> noneOf "\"\\"
                 char '"'
                 return $ String x

-- A atom starts with a letter of symbol followed by any number of letters, digits or symbols.
parseAtom :: Parser LispVal
parseAtom = do 
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

-- implementation for float, any number of digits, followed by a '.', then again any number of digits.
parseFloat :: Parser LispVal
parseFloat = do x <- many1 digit
                char '.'
                y <- many1 digit
                return $ Float (fst.head$readFloat (x++"."++y))

-- implementation for ratio, any number of digits, followed by '/', then again any number of digits.
parseRatio :: Parser LispVal
parseRatio = do x <- many1 digit
                char '/'
                y <- many1 digit
                return $ Ratio ((read x) % (read y))

-- A number can by any of the decimal representation or octal, hexadecimal, or binary number.
parseNumber :: Parser LispVal
parseNumber = parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin

-- A number may be represented as a digit or '#d' and then any number of digits. 
parseDecimal1 :: Parser LispVal
parseDecimal1 = many1 digit >>= (return . Number . read)

parseDecimal2 :: Parser LispVal
parseDecimal2 = do try $ string "#d"
                   x <- many1 digit
                   (return . Number . read) x

-- A hexadecmial number given by '#x' followed by allowed hexadecimal characters like 0-9,a-f. hex2dig is inlcuded in 'Numeric', which we have
-- imported before.
parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              x <- many1 hexDigit
              return $ Number (hex2dig x)

-- A octal number given by '#o' followed by allowed octal characters like 0-7. oct2dig is inlcuded in 'Numeric', which we have imported before.
parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              x <- many1 octDigit
              return $ Number (oct2dig x)
-- A binary number given by '#b' followed by allowed binary characters like 0,1.
parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              x <- many1 (oneOf "10")
              return $ Number (bin2dig x)

-- fst chooses the first argument, or a parameter in a data. $ is used for enlcosing readoct and x, '!! 0' chooses the first argument.
oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig  = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                         bin2dig' old xs

-- A list implementation. Given like (1 2). There has to be a space between values. sepBy spaces removes the spaces and parses the ParseExpr and
-- converts it to a valid LispVal by using liftM.
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- A DottedList implementation. Given like (1 2.2) or (1 (2.2)). There has to be a space between values. Converts it to a valid LispVal by using liftM.
parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

-- A list can also have quoted characters given like (1 '2) or (1 '(2 2)) and the quotes can also be ` or ,. The below three implementations are
-- for these.
parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

-- A vector is given like `#(1 '(2 2 2 2) "Anna"), it is a heterogeneous structures whose elements are indexed by numbers and take up less
-- space than a list.
parseVector :: Parser LispVal
parseVector = do arrayValues <- sepBy parseExpr spaces
                 return $ Vector (listArray (0,(length arrayValues - 1)) arrayValues)


-- ParseExpr can be any of the above types.
parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
	 <|> try parseComplex
         <|> try parseFloat 
	 <|> try parseRatio
	 <|> parseNumber
         <|> parseQuoted
	 <|> parseCharacter
	 <|> parseQuasiQuoted
       	 <|> parseUnQuote
	 <|> parseBool
	 <|> try (do string "#("
		     x <- parseVector
             	     char ')'
             	     return x)
	 <|> do char '('
		x <- try parseList <|> parseDottedList
                char ')'
                return x

-- eval takes in a parameter of type LispVal and return a monadic LispVal itself. The "_" is a don't care variable, which matches any value, yet
-- not binding it to the variable. It matches the val to the whole LispValue and not just the constructor of the variable.
-- So now we are outputting the data in the same format as we get, rather than changing it to the string representation. Here it is a monadic
-- repesentation. Suppose we have an Integer in 'java' with value 3 held in a variable 'a', then if we do 'show val' in Haskell it'll be somewhat
-- equivalent to a.toString() and then we are printing it. But if we use 'eval' function now, then we are converting the output we want to give as
-- an "Integer", before printing it. Can be thought as type casting the print statment for our data.
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
-- here we are implementing the basic calculations, we take an arguments and evaluate it and then map it by applying to func.
eval (List (Atom func : args)) = mapM eval args >>= apply func
-- error implementation.
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- Here we are taking a string which will be mapped to primitives, if it is present, then (operator like +, -, *...) will be applied
-- to args, if it is not defined in primitives (i.e. if operator is not there, then we return the appropriate error). Again the return type
-- here is a monad.
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

-- we are mapping the String and the list of args and evaluating them and returning a monadic value.
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+" , numericBinop (+)) ,
              ("-" , numericBinop (-)) ,
              ("*" , numericBinop (*)) ,
              ("/" , numericBinop div) ,
              ("mod" , numericBinop mod) ,
              ("quotient" , numericBinop quot) ,
              ("remainder" , numericBinop rem)]

-- Numeric Binary Operation takes a list of parameters like (- 2 3), and using fold1 converts it to (2 - 3) by unpacking each number in the list
-- to a Number. And this returns a monadic value, which will be equal to the result of the evaluation.
-- If there is only one operand or any other type mismatch, appropriate errors are given.
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

-- Unpacking the List of LispValues, if it is a number, it is returned as it is. If it is a string, then we parse it and select the first
-- argument from the read on Integer. read will return a "parsed value and the remaining Stirng", by choosing first value (fst), we take
-- parsed value. If we are not able to parse it, we return appropriate error. And also for list of values, unpack is called recursively.
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                           if null parsed 
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

-- ParseError is wrapped with LispError constructor Parser, and built-in throwError to return that in our ThrowError monad. And the readExpr
-- returns a monadic value, we need to wrap the other function in the return function.
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
     Left err -> throwError $ Parser err
     Right val -> return val

-- Main module, which is of type IO and takes the input from user and prints it accordingly. First the arguments from "getArgs" are taken, the
-- first argument is extracted and parsed (readExpr) and then evaluated with "eval" and calling show on it with error monad.
-- The evaled type will be "Either LispError String". LispError, if there is some error or if it is not then the value is extarcted and print.
main :: IO ()
main = do
     args <- getArgs
     evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
     putStrLn $ extractValue $ trapError evaled
