-- Add your names and student numbers to the following lines. Do not change anything else on these lines, since it they are parsed.
-- Student 1: Dragoș Erhan (s2940124)
-- Student 2: Ion Tulei (s2928787)
-- Student 3: Third student (szzzzzzz)

{-# LANGUAGE TemplateHaskell #-}

module MicroFP where

import Control.Applicative
import PComb
import BasicParsers
import Test.QuickCheck
import Test.QuickCheck.All
-- We use groupBy in patmatch from Data.List.
import Data.List
import Debug.Trace

-- FP3.1
-- Contributors: Dragoș Erhan, Ion Tulei
-- Set of data constructors that represent our EDSL for μFP.
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

-- FP3.2
-- Contributors: Dragoș Erhan
-- This section contains definitions for the following functions (functions.txt):
-- fibonacci, fib, sum, div, twice, add, inc, eleven.

fibonacci :: Prog
fibonacci = Prog
    [ Func "fibonacci" [Cons 0] (Cons 0)
    , Func "fibonacci" [Cons 1] (Cons 1)
    , Func "fibonacci" [Var "n"] (Add (Call "fibonacci" [Min (Var "n") (Cons 1)])
                                      (Call "fibonacci" [Min (Var "n") (Cons 2)]))
    ]

-- Expected: 89
fibonacciEx :: Integer
fibonacciEx = eval fibonacci "fibonacci" [11]

fib :: Prog
fib = Prog
    [ Func "fib" [Var "n"]
        (If (Smaller (Var "n") (Cons 3))
            (Cons 1)
            (Add (Call "fib" [Min (Var "n") (Cons 1)])
                 (Call "fib" [Min (Var "n") (Cons 2)])))
    ]

-- Expected: 89
fibEx :: Integer
fibEx = eval fib "fib" [11]

sumProg :: Prog
sumProg = Prog
    [ Func "sum" [Cons 0] (Cons 0)
    , Func "sum" [Var "a"] (Add (Call "sum" [Min (Var "a") (Cons 1)]) (Var "a"))
    ]

-- Expected: 45
sumProgEx :: Integer
sumProgEx = eval sumProg "sum" [9]

divProg :: Prog
divProg = Prog
    [ Func "div" [Var "x", Var "y"]
        (If (Smaller (Var "x") (Var "y"))
            (Cons 0)
            (Add (Cons 1)
                 (Call "div" [Min (Var "x") (Var "y"), Var "y"])))
    ]

-- Expected: 33
divProgEx :: Integer
divProgEx = eval divProg "div" [100, 3]

-- This is a higher order function (it does not work).
twice :: Prog
twice = Prog
    [ Func "twice" [Var "f", Var "x"] 
        (Call "f" [Call "f" [Var "x"]])
    ]

addIncEleven :: Prog
addIncEleven = Prog
    [ Func "add" [Var "x", Var "y"] (Add (Var "x") (Var "y"))
    , Func "inc" [Var "x"] (Call "add" [Cons 1, Var "x"])
    , Func "eleven" [] (Call "inc" [Cons 10])
    ]

-- Expected: [69, 70, 11]
addIncElevenEx :: [Integer]
addIncElevenEx = [eval addIncEleven "add" [41, 28], eval addIncEleven "inc" [69], eval addIncEleven "eleven" []]

-- The fourty function does not work (higher order).
fourty :: Prog
fourty = Prog
    [ Func "double" [Var "a"] (Mul (Var "a") (Cons 2))
    , Func "fourty" [] (Call "twice" [Var "double", Cons 10])
    ]

-- Expected: 66
fourtyEx :: Integer
fourtyEx = eval fourty "double" [33]

-- FP3.3
-- Contributors: Dragoș Erhan
-- Definition of a pretty printer that generates a textual
-- representation that corresponds to the grammar of μFP.

class PrettyPrint a where
    pretty :: a -> String

-- Define PrettyPrint instances for our EDSL data types.
instance PrettyPrint Prog where
    pretty (Prog funDefs) = unlines (map pretty funDefs)

instance PrettyPrint FunDef where
    pretty (Func name params expr) =
        name ++ " " ++ (unwords (map pretty params)) ++ " := " ++ pretty expr ++ ";"

instance PrettyPrint Expr where
    pretty (Var name) = name
    pretty (Cons value) = show value
    pretty (If order thenExpr elseExpr) =
        "if (" ++ pretty order ++
        ") then { " ++ pretty thenExpr ++ " } else { " ++ pretty elseExpr ++ " }"
    -- The parens in the Add Min and Mul is to maintain the order of the operations 
    pretty (Add left right) = "(" ++  pretty left ++ " + " ++ pretty right ++ ")"
    pretty (Min left right) = "(" ++ pretty left ++ " - " ++ pretty right ++ ")"
    pretty (Mul left right) = "(" ++ pretty left ++ " * " ++ pretty right ++ ")"
    pretty (Call funcName args) =
        funcName ++ "(" ++ (intercalate ", " (map pretty args)) ++ ")"

instance PrettyPrint Order where
    pretty (Smaller expr1 expr2)= pretty expr1 ++ " < " ++ pretty expr2
    pretty (Equals expr1 expr2)= pretty expr1 ++ " == " ++ pretty expr2
    pretty (Bigger expr1 expr2)= pretty expr1 ++ " > " ++ pretty expr2

-- Expected: 
-- "fibonacci 0 := 0;\nfibonacci 1 := 1;\nfibonacci n := (fibonacci((n - 1)) + fibonacci((n - 2)));\n"
prettyEx1 :: String
prettyEx1 = pretty fibonacci

-- "fib n := if (n < 3) then { 1 } else { (fib((n - 1)) + fib((n - 2))) };\n"
prettyEx2 :: String
prettyEx2 = pretty fib

-- "sum 0 := 0;\nsum a := (sum((a - 1)) + a);\n"
prettyEx3 :: String
prettyEx3 = pretty sumProg

-- "div x y := if (x < y) then { 0 } else { (1 + div((x - y), y)) };\n"
prettyEx4 :: String
prettyEx4 = pretty divProg

-- "add x y := (x + y);\ninc x := add(1, x);\neleven  := inc(10);\n"
prettyEx5 :: String
prettyEx5 = pretty addIncEleven

-- "double a := (a * 2);\nfourty  := twice(double, 10);\n"
prettyEx6 :: String
prettyEx6 = pretty fourty

-- FP3.4
-- FP5.2
-- Contributors: Dragoș Erhan
-- Define a evaluator for our μFP EDSL with support for pattern matching.

-- Evaluate a program by finding and executing the function with the given name and arguments.
eval :: Prog -> String -> [Integer] -> Integer
eval prog xs list = evalExpr ys prog
    where -- Find the function expression corresponding to the given name and arguments.
        ys = case findFunc prog xs list of
                  Nothing   -> error "No function found"
                  Just expr -> expr

-- Expected: 55
evalEx :: Integer
evalEx = eval fib "fib" [10]

-- Evaluate an expression within a program.
-- The function gets as a parameter the expression and the program which is needed when in the expression there is a function call.
-- In all the other cases the expression is processed accordingly, in sum cases recursively evaluating the sub-expressions.
evalExpr :: Expr -> Prog -> Integer
evalExpr (Var _) _ = error "Not enough param"
evalExpr (Cons n) _ = n
evalExpr (If o expr1 expr2) p
    | evalOrder o p = evalExpr expr1 p
    | otherwise = evalExpr expr2 p
evalExpr (Add expr1 expr2) p = evalExpr expr1 p + evalExpr expr2 p
evalExpr (Min expr1 expr2) p = evalExpr expr1 p - evalExpr expr2 p
evalExpr (Mul expr1 expr2) p = evalExpr expr1 p * evalExpr expr2 p
evalExpr (Call name expr_s) p = eval p name (map (\x -> evalExpr x p) expr_s)

-- Evaluate an order (condition) within a program.
-- The parameters represent the type class Order and the program which is going to be used by the evalExpr, as each constructor in the
-- type class has two expressions as arguments which in terms have to evaluated as well.  
evalOrder :: Order -> Prog -> Bool
evalOrder (Smaller expr1 expr2) p = evalExpr expr1 p < evalExpr expr2 p
evalOrder (Equals expr1 expr2) p = evalExpr expr1 p == evalExpr expr2 p
evalOrder (Bigger expr1 expr2) p = evalExpr expr1 p > evalExpr expr2 p

-- The findFunc functions searches the program for the function using the function name and the list of parameters and in turn returns
-- the expression of that function, that has already been applied bind. 
findFunc :: Prog -> String -> [Integer] -> Maybe Expr
findFunc (Prog []) _ _ = Nothing
findFunc (Prog ((Func name params expr):xs)) t list 
    | length params == length list = findFunc2 (checkParams params list) (Prog ((Func name params expr):xs)) t list
    | otherwise = findFunc (Prog xs) t list

-- Helper function findFunc2 checks whether the list of params corresponds to the list of params of the function being checked, if it does not 
-- correspond, them check the next function. The function returns the expression of the function that fits, also applying bind to it.
findFunc2 :: Maybe [(String, Integer)] -> Prog -> String -> [Integer] -> Maybe Expr
findFunc2 Nothing (Prog (_:xs)) t list = findFunc (Prog xs) t list
findFunc2 (Just res) (Prog ((Func name params expr):xs)) t list 
    | name == t && (length params) == (length list) = Just (bind expr (map fst res) (map snd res))
    | otherwise = findFunc (Prog xs) t list 

-- Check if parameter expressions match the given arguments.
-- This function checks if the given params fits the params of the function, as there can be pattern matching.
checkParams :: [Expr] -> [Integer] -> Maybe [(String, Integer)]
checkParams [] [] = Just []
checkParams ((Var n):xs) (y:ys) = (++) <$> Just [(n, y)] <*> checkParams xs ys
checkParams ((Cons n):xs) (y:ys) 
    | n == y = checkParams xs ys
    | otherwise = Nothing

-- Bind variables in an expression to their corresponding values.
-- The bind function gets a expression, a list of parameter names and a list of Integer, values, corresponding to those parameters.
-- The bind function checks in the first case if the expression is of type variable, if it is, then it changes it to a Cons with the value according,
-- it uses the getElem to find the corresponding integer value. If it finds Nothing, then it means that we have to leave the variable as it is.
bind :: Expr -> [String] -> [Integer] -> Expr
bind (Var n) names integer = 
    case s of
         Just m  -> Cons m
         Nothing -> Var n
    where s = getElem n names integer
-- In case the bind function gets a expression of type Cons n, we can leave it as it is, as this is the expression type of a simple value and don't needs changing.
-- In all the other cases we apply the bind function to the sub-expressions. 
bind (Cons n) _ _ = (Cons n)
bind (If ord expr1 expr2) names integers = If newOrder newExpr1 newExpr2
  where
    newExpr1 = bind expr1 names integers
    newExpr2 = bind expr2 names integers
    newOrder = case ord of
        Smaller expr3 expr4 -> Smaller (bind expr3 names integers) (bind expr4 names integers)
        Bigger expr3 expr4  -> Bigger (bind expr3 names integers) (bind expr4 names integers)
        Equals expr3 expr4  -> Equals (bind expr3 names integers) (bind expr4 names integers)

bind (Add expr1 expr2) names integer = Add (bind expr1 names integer) (bind expr2 names integer)
bind (Min expr1 expr2) names integer = Min (bind expr1 names integer) (bind expr2 names integer)
bind (Mul expr1 expr2) names integer = Mul (bind expr1 names integer) (bind expr2 names integer)
bind (Call n expr_s) names integer = Call n (map (\x -> bind x names integer) expr_s)

-- This helper function helps return the corresponding value of the variable by checking the list of variable names and the list of values linked to it.
-- If the variable is not in the list of variables it returns Nothing, in any other case it returns Just Integer(value) of that variable.
getElem :: String -> [String] -> [Integer] -> Maybe Integer
getElem n [] [] = Nothing
getElem n (x:xs) (y:ys)
    | n == x = Just y
    | otherwise = getElem n xs ys

-- FP4.1
-- Contributors: Ion Tulei
-- This parser parses an entire program.
-- It constructs a Prog object containing a list of functions. 
prog :: Parser Prog
prog =  Prog <$> ((:) <$> func <*> many func) 
    <|> failure

-- Expected: 
-- [(Prog [Func "fibonacci" [Var "n"] (Add (Call "fibonacci" [Min (Var "n") (Cons 1)]) (Call "fibonacci" [Min (Var "n") (Cons 2)]))],Stream "")]
progEx :: [(Prog, Stream)]
progEx = runParser prog $ Stream "fibonacci n := fibonacci (n-1) + fibonacci (n-2);"

-- This parser parses a function definition.
func :: Parser FunDef
func =  Func <$> identifier 
             <*> many (Var <$> identifier <|> Cons <$> integer) 
             <*> (symbol ":=" *> expr <* symbol ";")
    <|> failure

-- Expected: 
-- [(Func "fibonacci" [Var "n"] (Add (Call "fibonacci" [Min (Var "n") (Cons 1)]) (Call "fibonacci" [Min (Var "n") (Cons 2)])),Stream "")]
funcEx :: [(FunDef, Stream)]
funcEx = runParser func $ Stream "fibonacci n := fibonacci (n-1) + fibonacci (n-2);"

-- This parser parses an expression.
-- Falls back to parsing a term if no addition or substraction was found.
expr :: Parser Expr
expr =  Add <$> term <*> (symbol "+" *> expr)
    <|> Min <$> term <*> (symbol "-" *> expr)
    <|> term
    <|> failure

-- Expected:
-- [(Add (Call "fibonacci" [Min (Var "n") (Cons 1)]) (Call "fibonacci" [Min (Var "n") (Cons 2)]),Stream "")]
exprEx :: [(Expr, Stream)]
exprEx = runParser expr $ Stream "fibonacci (n-1) + fibonacci (n-2)"

-- This parser parses a term.
-- Falls back to parsing a factor if no multiplication found.
term :: Parser Expr
term =  Mul <$> factor <*> (symbol "*" *> term)
    <|> factor
    <|> failure

-- Expected:
-- [(Mul (Call "fibonacci" [Min (Var "n") (Cons 1)]) (Cons 2),Stream "")]
termEx :: [(Expr, Stream)]
termEx = runParser term $ Stream "fibonacci (n-1) * 2"

-- This parser parses a factor.
-- Parses constants, function calls, if expressions, variables, or parenthesized expressions.
factor :: Parser Expr
factor =  Cons <$> integer
      <|> Call <$> identifier <*> (parens (sep1 expr (char ',')))
      <|> If <$> (symbol "if"   *> parens order) 
             <*> (symbol "then" *> braces expr) 
             <*> (symbol "else" *> braces expr)
      <|> Var <$> identifier
      <|> parens expr
      <|> failure

-- Expected:
-- [(If (Smaller (Var "x") (Var "y")) (Cons 0) (Add (Cons 1) (Call "div" [Min (Var "x") (Var "y"),Var "y"])),Stream "")]
factorEx :: [(Expr, Stream)]
factorEx = runParser factor $ Stream "if (x < y) then { 0 } else { 1 + div ((x-y), y) }"

-- This parser parses a comparison.
order :: Parser Order
order =  Smaller <$> expr <*> (symbol "<" *> expr)
     <|> Equals <$> expr <*> (symbol "==" *> expr)
     <|> Bigger <$> expr <*> (symbol ">" *> expr)
     <|> failure

-- Expected:
-- [(Equals (Add (Call "fib" [Min (Var "n") (Cons 1)]) (Cons 10)) (Min (Var "x") (Cons 21)),Stream "")]
orderEx :: [(Order, Stream)]
orderEx = runParser order $ Stream "(fib(n-1) + 10) == ( x - 21)"

-- FP4.2
-- Contributors: Ion Tulei
-- This function compiles a string into a Prog.
compile :: String -> Prog
compile xs = res
    where [(res, str)] = runParser prog (Stream xs)

-- Expected:
-- Prog [Func "fibonacci" [Var "n"] 
--                        (Add (Call "fibonacci" [Min (Var "n") (Cons 1)]) 
--                        (Call "fibonacci" [Min (Var "n") (Cons 2)])),
--       Func "baf" [Var "x",Var "y"] 
--                  (If (Smaller (Var "x") (Var "y")) 
--                               (Cons 0) 
--                               (Add (Cons 1) (Call "div" [Min (Var "x") (Var "y"),Var "y"])))]
compileEx1 :: Prog
compileEx1 = compile 
    "fibonacci n := fibonacci (n-1) + fibonacci (n-2); baf x y := if (x < y) then { 0 } else { 1 + div ((x-y), y) };"

-- Expected: too much to include here; runFileEx confirms compile works as intended.
compileEx2 :: IO Prog
compileEx2 = compile <$> readFile "functions.txt"

-- FP4.3
-- Contributors: Ion Tulei
-- This function reads a file, compiles it, and evaluates the last function in it with a list of integers.
runFile :: FilePath -> [Integer] -> IO Integer
runFile xs list = fmap (\prog -> eval prog (getLastFuncName prog) list) (compile <$> readFile xs)

-- Expected: 499
-- Last argument is an empty list because the last function in 'functions.txt' is main.
runFileEx :: IO Integer
runFileEx = runFile "functions.txt" []

-- Get the name of the last function in the program.
-- Return '404' if no functions provided.
getLastFuncName :: Prog -> String
getLastFuncName (Prog funcs)
    | null funcs = "404"
    | otherwise  = getFuncName $ last funcs

-- Get the name of a function.
getFuncName :: FunDef -> String
getFuncName = \(Func name _ _) -> name

-- FP5.3
-- Contributors: Ion Tulei 
-- This function rewrites function definitions with 
-- pattern matching to a single definition with if expressions.
patmatch :: Prog -> Prog
patmatch (Prog funcs) = Prog (rewriteFunc <$> groupBy sameName funcs)

-- Expected:
-- "fibonacci n := if (0 == n) then { 0 } else { if (1 == n) then { 1 } else { (fibonacci((n - 1)) + fibonacci((n - 2))) } };\n"
patmatchEx1 :: String
patmatchEx1 = pretty $ patmatch $ compile "fibonacci 0 := 0;fibonacci 1 := 1;fibonacci n := fibonacci (n-1) + fibonacci (n-2);"

-- Expected: 55
patmatchEx2 :: Integer
patmatchEx2 = eval (patmatch $ compile 
    "fibonacci 0 := 0;fibonacci 1 := 1;fibonacci n := fibonacci (n-1) + fibonacci (n-2);") "fibonacci" [10]

-- Expected: 
-- "sum a := if (0 == a) then { 0 } else { (sum((a - 1)) + a) };\n"
patmatchEx3 :: String
patmatchEx3 = pretty $ patmatch $ compile "sum 0 := 0;sum a := sum (a-1) + a;"

-- Expected: 78
patmatchEx4 :: Integer
patmatchEx4 = eval (patmatch $ compile "sum 0 := 0;sum a := sum (a-1) + a;") "sum" [12]

-- Checks if two functions have the same name.
-- This function is used in the groupBy clause in patmatch to group functions by their names.
-- ex: [[fib ..., fib ..., fib, ...], [sum ..., sym ..., sum ...]]
sameName :: FunDef -> FunDef -> Bool
sameName (Func name1 _ _) (Func name2 _ _) = name1 == name2

-- Rewrites multiple function definitions with the same name into a single function.
rewriteFunc :: [FunDef] -> FunDef
rewriteFunc funcs@(Func name args _:_) = Func name [var] (createBody funcs var)
    where var = getLastFuncVar funcs

-- Creates the body of the new (concatinated) single function.
createBody :: [FunDef] -> Expr -> Expr
createBody [Func _ _ body] _ = body
createBody (Func _ (val:_) body:fs) var = If (Equals val var) (body) (createBody fs var)

-- Get the first variable used in the last function definition.
-- We assume that function definitions are ordered
-- such that base cases come first, and the general case is the last definition. 
getLastFuncVar :: [FunDef] -> Expr
getLastFuncVar = getFuncVar . last 

-- Get the first variable used by a function. 
getFuncVar :: FunDef -> Expr
getFuncVar (Func _ (v:_) _) = v

-- FP5.6
-- Contributors: Ion Tulei
-- These Arbitrary instances are used to generate random data for testing our EDSL.

instance Arbitrary Prog where
    arbitrary = sized $ \n -> Prog <$> vectorOf (min 3 $ max 1 n) arbitrary

instance Arbitrary FunDef where
    arbitrary = sized $ \n -> Func <$> genFuncName <*> argsGen <*> exprGen (min 3 n)
        where x = 2

instance Arbitrary Expr where
    arbitrary = sized exprGen

instance Arbitrary Order where
    arbitrary = orderGen 1

-- Generates a list of arguments.
-- Equal odds of generating 1, 2, or 3 arguments.
argsGen :: Gen [Expr]
argsGen = oneof [ vectorOf 1 (exprGen 0)
                , vectorOf 2 (exprGen 0)
                , vectorOf 3 (exprGen 0)
                ]

-- Generates an expression.
-- We are using the idea from the lecture to ensure the generator always terminates.
exprGen :: Int -> Gen Expr
exprGen 0 = oneof [ Var <$> genIdName
                  , Cons <$> abs <$> arbitrary
                  ]
exprGen n = oneof [ Var <$> genIdName
                  , Cons <$> abs <$> arbitrary
                  , If <$> arbitrary <*> subExpr <*> subExpr
                  , Add <$> subExpr <*> subExpr
                  , Min <$> subExpr <*> subExpr
                  , Mul <$> subExpr <*> subExpr
                  , Call <$> genFuncName <*> argsGen
                  ]
    where
        m = min 3 $ n `div` 2
        subExpr = exprGen m

-- Generates order expressions.
-- Equal odds of generating a smaller, equals, or bigger condition.
orderGen :: Int -> Gen Order
orderGen m = oneof [ Smaller <$> exprGen (min 3 n) <*> exprGen (min 3 n)
                   , Equals <$> exprGen (min 3 n) <*> exprGen (min 3 n)
                   , Bigger <$> exprGen (min 3 n) <*> exprGen (min 3 n)
                   ]
    where n = m `div` 2

-- Generates function names.
genFuncName :: Gen String
genFuncName = vectorOf 3 $ elements "fghk"

-- Generates identifier names.
genIdName :: Gen String
genIdName = vectorOf 3 $ elements "abcde"

-- Test if pretty printing an expression and 
-- then parsing it results in the original expression.
prop_prettyPrintExpr :: Expr -> Bool
prop_prettyPrintExpr ex = res == ex
    where [(res, _)] = runParser expr $ Stream $ pretty ex

-- Test if pretty printing a program and
-- then parsing it results in the original program.
prop_prettyPrintProg :: Prog -> Bool
prop_prettyPrintProg prog = res == prog
    where res = compile $ pretty prog

-- QuickCheck: all prop_* tests
return []
check = $quickCheckAll
