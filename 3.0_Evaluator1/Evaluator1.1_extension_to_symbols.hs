-- importing Parsec without spaces, as we will be using it later.
import Text.ParserCombinators.Parsec hiding(spaces)
import Control.Monad
import System.Environment
-- importing numeric method implementations, alomg with Ratio and Complex.
import Numeric
import Data.Ratio
import Data.Complex
-- for using vectors
import Data.Array

--Any instance that is type of "show", can be converted to string. We are extending the same for LispVal, by making it a member of "Show".
instance Show LispVal where show = showVal

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

-- eval takes in a parameter of type LispVal and return a LispVal itself. The "_" is a don't care variable, which matches any value, yet not
-- binding it to the variable. It matches the val to the whole LispValue and not just the constructor of the variable.
-- So now we are outputting the data in the same format as we get, rather than changing it to the string representation.
-- Suppose we have an Integer in 'java' with value 3 held in a variable 'a', then if we do 'show val' in Haskell it'll be somewhat equivalent
-- to a.toString() and then we are printing it. But if we use 'eval' function now, then we are converting the output we want to give as an
-- "Integer", before printing it. Can be thought as type casting the print statment for our data.
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
-- here we are implementing the basic calculations, we take an arguments and evaluate it and then map it by applying to func.
eval (List (Atom func : args)) = apply func $ map eval args

-- Here we are taking a string which will be mapped to primitives, if it is present, then (operator like +, -, *...) will be applied
-- to args, if it is not defined in primitives (i.e. if operator is not there, then we return false.)
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

-- we are mapping the String and the list of args and evaluating them and returning a lisp value.
primitives :: [(String , [LispVal] -> LispVal)]
primitives = [("+" , numericBinop (+)) ,
              ("-" , numericBinop (-)) ,
              ("*" , numericBinop (*)) ,
              ("/" , numericBinop div) ,
              ("mod" , numericBinop mod) ,
              ("quotient" , numericBinop quot) ,
              ("remainder" , numericBinop rem) ,
-- checking if an operand is symbol, string, number, boolean or a list.
              ("symbol?" , unaryOp symbolp) ,
              ("string?" , unaryOp stringp) ,
	      ("symbol2string?" , unaryOp symbol2stringp) ,
	      ("string2symbol?" , unaryOp string2symbolp) ,
              ("number?" , unaryOp numberp) ,
              ("bool?", unaryOp boolp) ,
              ("list?" , unaryOp listp)]

-- unary operator implementation.
unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v

-- definitions for symbol, bool, number, string, list, which takes a LispValue and gives a LispValue(true or false.)
symbolp, numberp, stringp, boolp, listp, symbol2stringp, string2symbolp :: LispVal -> LispVal
symbolp (Atom _)   = Bool True
symbolp _          = Bool False
numberp (Number _) = Bool True
numberp _          = Bool False
stringp (String _) = Bool True
stringp _          = Bool False
boolp   (Bool _)   = Bool True
boolp   _          = Bool False
listp   (List _)   = Bool True
listp   (DottedList _ _) = Bool True
listp   _          = Bool False
symbol2stringp (Atom s)   = String s
symbol2stringp _          = Bool False
string2symbolp (String s) = Atom s
string2symbolp _          = Bool False

-- Numeric Binary Operation takes a list of parameters like (- 2 3), and using fold1 converts it to (2 - 3) by unpacking each number in the list
-- to an Integer. And this returns a LispValue, which will be equal to the result of the evaluation.
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

-- Unpacking the List of LispValues, if it is a number, it is returned as it is. If it is a string, then we parse it and select the first
-- argument from the read on Integer. read will return a "parsed value and the remaining Stirng", by choosing first value (fst), we take
-- parsed value. If we are not able to parse it, we return zero. And also for list of values, unpack is called recursively.
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in 
                           if null parsed 
                              then 0
                              else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

-- read the expr given by the user, and senf it for parsing, and display the output. It takes an input of String and returns an output of type
-- String.
readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

-- Main module, which is of type IO and takes the input from user and prints it accordingly. First the arguments from "getArgs" are taken, the
-- first value of it is taken from "head", it is parsed with "readExpr", and then evaluated with "eval" and printed out.
main :: IO ()
main = getArgs >>= print . eval . readExpr . head
