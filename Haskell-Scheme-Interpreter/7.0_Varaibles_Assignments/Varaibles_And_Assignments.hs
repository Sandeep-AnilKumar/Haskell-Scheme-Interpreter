{-# LANGUAGE ExistentialQuantification #-}
import System.IO
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
-- for using assignments and variables.
import Data.IORef
type Env = IORef [(String, IORef LispVal)]
-- Monad transformers, that help to combine multiple monads.
type IOThrowsError = ErrorT LispError IO

-- IORef can only be used from within the IO Monad, so we have to create an empty environment. We cant use [] empty, since we want all IORef's
-- to be sequenced.
nullEnv :: IO Env
nullEnv = newIORef []

--This uses our previously-defined trapError function to take any error values and convert them to their string representations, then runs the
-- whole computation via runErrorT. The result is passed into extractValue and returned as a value in the IO monad.
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

-- We cannot use two actions cannot be used in the same monad, so we have to use lifting, to lift the lower type IO to combined Monad.
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

-- Checking if a variable is bound in that environment. we need to use the const function because maybe expects a function to perform on the
-- result and not just a value.
isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

-- Getting the variable from the environment. getVar uses the IOThrowsError monad, because it also needs to do some error handling. As a result,
-- we need to use the liftIO function to lift the readIORef action into the combined monad. 
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)

-- If a variable is already bound in that environment, then we are setting its value. Flip is used to flip the arguments for writeIORef.
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

-- We are defining a new var and value. If it is already bound, then we call setVar function, if not then we append the new var, value pair
-- in front of the list of the env.
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value

-- Binding all the variables in the environment, by extending it to the current bindings.
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)

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

-- Unpacker combines all the unpacking techniques like UnpackNum, UnpackString and UnpackBool.
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

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
	       deriving Eq

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
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
-- if-clause evaluator. pred is predicate, which will result in true or false on evaluation. If it is true conseq is evaluated, alt if it is
-- false if predicate is not true or false, then error is thrown.
eval env (List [Atom "if", pred, conseq, alt]) =
     do result <- eval env pred
     	case result of
             Bool False -> eval env alt
             otherwise -> eval env conseq
-- setting the variable.
eval env (List [Atom "set!", Atom var, form]) =
     eval env form >>= setVar env var
-- defining the variable.
eval env (List [Atom "define", Atom var, form]) =
     eval env form >>= defineVar env var
eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

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
              ("remainder" , numericBinop rem) ,
-- relational operators
	      ("=", numBoolBinop (==)) ,
	      ("<", numBoolBinop (<)) ,
	      (">", numBoolBinop (>)) ,
	      ("/=", numBoolBinop (/=)) ,
	      (">=", numBoolBinop (>=)) ,
	      ("<=", numBoolBinop (<=)) ,
	      ("&&", boolBoolBinop (&&)) ,
	      ("||", boolBoolBinop (||)) ,
	      ("string=?", strBoolBinop (==)) ,
	      ("string<?", strBoolBinop (<)) ,
	      ("string>?", strBoolBinop (>)) ,
	      ("string<=?", strBoolBinop (<=)) ,
	      ("string>=?", strBoolBinop (>=)) ,
-- car, cdr and cons are used with Lists and DottedLists.
	      ("car", car) ,
	      ("cdr", cdr) ,
              ("cons", cons) ,
-- eq and eqv are almost same in their operation. equal has a different approach than the former.
              ("eq?", eqv) ,
              ("eqv?", eqv) ,
	      ("equal?", equal)]

-- Numeric Binary Operation takes a list of parameters like (- 2 3), and using fold1 converts it to (2 - 3) by unpacking each number in the list
-- to a Number. And this returns a monadic value, which will be equal to the result of the evaluation.
-- If there is only one operand or any other type mismatch, appropriate errors are given.
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

-- takes in an argument of unpacker, which will unpack numbers, strings and bools. second parameter is operator, and third argument are
-- operands. It will return a monadic LispValue.
boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

-- unpacking approaches for number, string and bool.
numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

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

-- unpacking to string. If argument is a number or bool, it is converted to String using show.
unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

-- unpacking to bool.
unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

-- returns the first item in a list. If the argument is not a list or if there are more than one arguments, error is thrown.
car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

-- returns the "list - head". i.e. returns whatever is left after removing the head. If the argument is not a list or if there are more than one
-- arguments, error is thrown.
cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

-- puts the first argument in front of a list or dottedlist. Returns error of the arguments are not valid. If the two arguments are not lists
-- then the dottedlist representation of the two is given as output.
cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

-- Checks whether the two items in the list are equal. The items can be anything from bool to list and dottedlist, etc.
-- For a list, eqvList is called.
eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [l1@(List arg1), l2@(List arg2)] = eqvList eqv [l1, l2]
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

-- unpackEquals takes three arguments, the first two arguments are unpacked by the third arguments, for all unpacking techniques and are
-- compared if they are equal.
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

-- first the length of both the lists are checked and later the lists are zipped for all pairs and recursively checked for
-- equivalence. eqvpair is a function definition which is local to eqv function, given by the where clause.
eqvList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqvFunc [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && 
                                                    (all eqvPair $ zip arg1 arg2)
      where eqvPair (x1, x2) = case eqvFunc [x1, x2] of
                                    Left err -> False
                                    Right (Bool val) -> val


-- Checks if (2 "2") is equal. Ideally it should be equal, but eq and eqv returns false. So to test for a stricter equivalence, all unpacking
-- methods are combined and the arguments are checked if they are equal. "or" is passed if any of the combinations is true, that means
-- the two values are equal. Appropriate errors are passed if the arguments are less or more or are invalid.
equal :: [LispVal] -> ThrowsError LispVal
equal [l1@(List arg1), l2@(List arg2)] = eqvList equal [l1, l2]
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [arg1, arg2] = do
     primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                        [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
     eqvEquals <- eqv [arg1, arg2]
     return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

-- ParseError is wrapped with LispError constructor Parser, and built-in throwError to return that in our ThrowError monad. And the readExpr
-- returns a monadic value, we need to wrap the other function in the return function.
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
     Left err -> throwError $ Parser err
     Right val -> return val

-- flushes the output as soon as there is one.
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- flushes a prompt and gets an input from the user.
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- Evaluating string to catch if there are any errors, or/and extracting the result and returning it. The errors are trapped out of main into its
-- own function.
evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

-- evalutaing the Expression and printing it.
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

-- In haskell underscore for names are monadic functions that repeat but do not return a value. The until_ functions takes a predicate to see
-- if it is equivalent to "quit", if not then the prompt is read and the action is performed.
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
   result <- prompt
   if pred result 
      then return ()
      else action result >> until_ pred prompt action

-- During the start the environment is null. And the expr is evaluated.
runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

-- REPL runs the until_ function with all the parameters, and evaluates the expression if the predicate is not equal to quit.
runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

-- Main module, which is of type IO and takes the input from user and prints it accordingly. First the arguments from "getArgs" are taken, the
-- first argument is extracted and parsed (readExpr) and then evaluated with "eval" and calling show on it with error monad. If there are no
-- arguments then REPL is run. If there is one argument, then runOne is called to evaluate the expression.
main :: IO ()
main = do args <- getArgs
          case length args of
               0 -> runRepl
               1 -> runOne $ args !! 0
               otherwise -> putStrLn "Program takes only 0 or 1 argument"
