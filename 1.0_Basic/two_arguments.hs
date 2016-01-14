module Main where
 import System.Environment
 
 main :: IO ()
 main = do
     args <- getArgs
     putStrLn ("Hello, " ++ args !! 0 ++ " and Mark and " ++ args !! 1)
     putStrLn("give inputs from console")
     one <- getLine
     two <- getLine
     putStrLn(show(read one + read two))
