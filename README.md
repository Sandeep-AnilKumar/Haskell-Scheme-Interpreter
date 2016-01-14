"# Haskell-Scheme-Interpreter" 

Hello, welcome to my scheme-interpreter.

After unzipping the folder, you should be having the following folders and files in the directory containing this README.

1.0_Basic
2.0_Parser
3.0_Evaluator1
4.0_Errors_exceptions
5.0_Evaluator2
6.0_REPL
7.0_Variables_Assignments
8.0_Functions
Scheme-interpreter-final
README.txt

The first 8 folders consits of how I built the final interpreter which is inside the folder "Scheme-interpreter-final". They correspond to the
chapters in the book "https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours". They also have excercises solved in them. And each code
is well commented. Any beginner can use these folders and codes to understand how to build an interpreter from scratch in Haskell for Scheme.

Coming to installing the haskell compiler, we will have to do the following steps to install it.

Go to terminal (This tutorial and README gives instructions on how to install and compile and write code in Ubuntu).

type in "sudo apt-get install haskell-platform" (without the quotes.) Or type in the same given at "https://www.haskell.org/platform/#linux-ubuntu"
next we will have to install "Parsec" library, as we will be using that.
type in "sudo apt-get install cabal-install"
followed by "cabal update"
and lastly "cabal install parsec". All without the quotes. You can also look at the following link for more information on Parsec and Cabal
"https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing"

So now we have the compiler ready.
If you have extracted the zip folder in say /home/Mark/Downloads/HomeWork2/
go to terminal and type "cd /home/Mark/Downloads/HomeWork2/1.0_Basic"
read the program "first_program.hs"
and type the following in terminal "ghc -o first_program --make first_program.hs" and this will compile the program and run it as below:-
enter the following in the terminal "./first_program Mark" and hit enter.
Here Mark is the argument and it should say "Hello, Mark"

So this is all about compilation. Go through all the chapters and codes and understand it, it is fairly well commented.

In the end we will end up with the haskell file in the folder "Scheme-interpreter-final"
The final interpreter is "Scheme-Interpreter.hs".

The folder also has a scheme library by name "stdlib.scm"

We can now enter the following in terminal

ghc -o scheme-interpreter --make Scheme-Interpreter.hs
./scheme-interpreter


after hitting the above two command we will enter the REPL developed for Scheme. And you will see something like
Lisp>>>

go on and type 

Lisp>>> (load "stdlib.scm").

This will load the scheme library and we can use the functions to do something like
Lisp>>> (map (curry + 2) '(1 2 3 4))
Lisp>>> (3 4 5 6)

The first command maps the result of currying 2 to the list containing '(1 2 3 4)
giving the output in the next line.

type "quit" to get out of REPL.

Lisp>>> quit


If we give an argument with ./scheme-interpreter stdlib.scm
The argument is parsed and executed. The argument will be taken as the file to be loaded. It takes only scheme files as arguments.
If no file is found appropriate errors are given. It takes 0 arguments (to enter into REPL). Or one to execute the given expression given in the
argument.

Go on and have fun with the interpreter and CURRY your own Functions.

If you have any difficulties in executing any of the commands or codes, reach out to me at
EMAIL :- sandeepkanil@gmail.com
PHONE :- (312) 468-5815

Thank you!
