# Functional Programming Project - µFP Language Parser and Evaluator

**Module 8 - Programming Paradigms, Bachelor of Technical Computer Science, University of Twente**

**Date:** 13-06-2024

**Contributors:**  
- [Dragoș Erhan](https://github.com/Headpoint2042)
- [Ion Tulei](https://github.com/iontulei)

## Project Overview

This project involves developing a parser and evaluator for **µFP (microFP)**, a simple functional programming language. The goal is to implement key components of the language using Haskell, including:
- A **parser combinator library**.

- **Basic parsers** for handling µFP language constructs.

- An **Embedded Domain Specific Language (EDSL)** for µFP.

- A **µFP evaluator** to execute µFP programs.

- A **parser** for µFP's grammar.

The µFP language is a minimalistic functional language designed for this project. It supports functions with multiple arguments, pattern matching, arithmetic operations, and conditional expressions. The goal is to implement a parser for this language, an evaluator to execute µFP programs, and an EDSL to represent µFP in Haskell.

The project revisits many fundamental concepts such as parser combinators, higher-order functions, and functional programming paradigms.

## µFP Language Features

µFP follows a simple grammar:

- Functions must end with a semicolon.

- Function application is done with parentheses around comma-separated arguments.

- `if-else` expressions use parentheses for conditions and braces for branches.

Example µFP function definitions:
```haskell
fibonacci 0 := 0;
fibonacci 1 := 1;
fibonacci n := fibonacci (n-1) + fibonacci (n-2);

sum 0 := 0;
sum a := sum (a-1) + a;

main := sum (10);
```

## Project Structure

### 1. Parser Combinator Library (`PComb.hs`)

We implemented a custom parser combinator library that can:

- Parse character streams.

- Support Functor, Applicative, and Alternative instances.

- Handle combinators such as `char`, `failure`, and sequence parsers.

**Key Features:**
- **FP1.1**: Defined the Parser data type as a wrapper around a function that takes a Stream and returns a list of possible parses, each with a result of type r and a remaining Stream.
    ```haskell
    data Parser r = P {
        runParser :: Stream -> [(r, Stream)]
    }
    ```
- **FP1.2**: Implemented Functor instance for Parser.
    ```haskell
    instance Functor Parser where
    fmap f p = P (\x -> [(f r, xs) | (r, xs) <- runParser p x])
    ```
- **FP1.3**: Defined a parser that consumes a single specific character.
    ```haskell
    char :: Char -> Parser Char
    char c = P p
        where
            p (Stream [])                 = [] 
            p (Stream (x:xs)) | c == x    = [(x, (Stream xs))]
                              | otherwise = []
    ```
- **FP1.5**: Applicative instance for parsers.
    ```haskell
    instance Applicative Parser where
    pure a = P $ \xs -> [(a, xs)]
    p1 <*> p2 = P (\s -> [(r1 r2, s2) | (r1, s1) <- runParser p1 s, (r2, s2) <- runParser p2 s1])
    ```

- **FP1.6**: Alternative instance to try multiple parsers in sequence.
    ```haskell
    instance Alternative Parser where
    empty = failure
    p1 <|> p2 = P (\s -> case runParser p1 s of
                              [] -> runParser p2 s
                              res -> res)
    ```              

### 2. Basic Parsers (`BasicParsers.hs`)

We developed essential parsers to interpret µFP's syntax.

- **letter** and **digit** parsers to handle basic characters and numbers.

- **identifier**, **integer**, and **string** parsers to handle µFP's grammar constructs.

- Other combinators like `sep`, `option`, and `whitespace`.

**Key Features:**

- **FP2.1**: `letter` and `digit` parsers to handle basic characters and numbers.

- **FP2.2**: Parser combinators such as `between` and `whitespace`.

- **FP2.3**: Parsers like `sep`, `sep1`, `option`.

- **FP2.4**: Parsers like `identifier`, `integer`, `parens`, and `braces` to handle µFP syntax.


### 3. µFP Embedded Domain-Specific Language (`MicroFP.hs`)

We designed a deeply embedded domain-specific language (EDSL) to represent µFP programs. This EDSL handles function definitions, arithmetic expressions, and control flow constructs like `if-else`.

**EDSL Features:**

- **FP3.1**: Type constructors for µFP, including support for functions, expressions, and terms.
    ```haskell
    data Prog = Prog [FunDef] 
              deriving (Show, Eq)

    data FunDef = Func String [Expr] Expr
                deriving (Show, Eq)
    
    data Expr = Var String
              | Cons Integer
              | If Order Expr Expr
              | Add Expr Expr
              | Min Expr Expr
              | Mul Expr Expr
              | Call String [Expr]
              deriving (Show, Eq)
    
    data Order = Smaller Expr Expr 
               | Equals Expr Expr 
               | Bigger Expr Expr 
               deriving (Show, Eq)
    ```

- **FP3.2**: µFP functions such as `fibonacci`, `sum`, `twice`, and `div` were implemented.

- **FP3.3**: A pretty printer that generates a textual representation that corresponds to the grammar of the μFP.

- **FP3.4**: An evaluator that interprets and executes µFP programs, with support for multiple arguments , simple arithmetic operations (`+`, `-`, `*`), and pattern matching. Example:
    ```haskell
    eval fibonacci [10]
    ```

### 4. µFP Parser

The parser converts µFP source code into the EDSL. 

**Key Features:**

- **FP4.1:** Parsers for `factor`, `expr`, `term`, etc. to handle µFP syntax.

- **FP4.2:** `compile :: String -> Prog` to parse and translate µFP code into the EDSL.

- **FP4.3:** `runFile :: FilePath -> [Integer] -> IO Integer` to compile and evaluate µFP code from a file.

### 5. µFP Additional Features

Further, bonus features were implemented to enhance the functionality of the µFP evaluator.

**Key Features:**

- **FP5.2:** Implemented pattern matching for `eval` using substitution. The μFP functions `sum` and `fibonacci` work correctly.

- **FP5.3:** `patmatch` function that rewrites function definitions with pattern
matching (in our EDSL) to a single definition with if-expressions. The μFP functions `sum` and `fibonacci` work correctly.

- **FP5.6:** Automated testing for our compiler framework using `QuickCheck`. The tests ensure:
    - Correct parsing of µFP code.
    - Proper evaluation of µFP functions and expressions.

### How to Run

1. Compile the project using `GHC`.

2. Load a µFP program from a file using the `runFile` function:
    ```bash
    runFile "path/example.txt" [args]
    ```
    This will parse, compile, and evaluate the µFP code.
    
### Example µFP Program

Here is a simple µFP program (fib.txt) that calculates the Fibonacci sequence:

```haskell
fibonacci 0 := 0;
fibonacci 1 := 1;
fibonacci n := fibonacci (n-1) + fibonacci (n-2);
```

To run this program using the evaluator:
```bash
runFile "path/fib.txt" [10]
```

This will compute the 10th Fibonacci number using the µFP language.

### Conclusion

This project demonstrates the implementation of a functional programming language and highlights core concepts such as parser combinators, EDSL design, and functional evaluation. The µFP language provides a simplified environment to practice these concepts, and the project showcases how to build an interpreter from the ground up using Haskell.



