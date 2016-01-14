module Main where
 import System.Environment
 -- this is the main module, from where execution starts. We are defining main as a type of IO, to do basic input output operations.
 main :: IO ()
 main = do
-- args are got by getArgs, and are stored in args. Later println is used to print the first argument i.e. args[0], by using !! 0 notation in Haskell.
     args <- getArgs
     putStrLn ("Hello, " ++ args !! 0)
