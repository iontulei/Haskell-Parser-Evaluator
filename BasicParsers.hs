-- Add your names and student numbers to the following lines. Do not change anything else on these lines, since they are parsed.
-- Student 1: Dragoș Erhan (s2940124)
-- Student 2: Ion Tulei (s2928787)
-- Student 3: Third student (szzzzzzz)

module BasicParsers where

import Control.Applicative
import Data.Char
import Test.QuickCheck
import PComb

-- FP2.1
-- Contributors: Dragoș Erhan
-- Define a parser that parses a single letter.
letter :: Parser Char
letter = foldr (<|>) empty (map char (['a'..'z'] ++ ['A'..'Z']))

-- Expected: [('a',Stream "12")]
letterEx :: [(Char, Stream)]
letterEx = runParser letter (Stream "a12")

-- Define a parser that parses a single digit.
dig :: Parser Char
dig = foldr (<|>) empty (map char (['0'..'9']))

-- Expected: [('1',Stream "23")]
digEx :: [(Char, Stream)]
digEx = runParser dig (Stream "123") 

-- FP2.2: Define a parser that parses something between two other parsers.
between :: Parser a -> Parser b -> Parser c -> Parser b
between a b c = a *> b <* c

-- Expected: [('4',Stream "")]
betweenEx :: [(Char, Stream)]
betweenEx = runParser (between letter dig letter) (Stream "b4b")

-- Define a parser that ignores surrounding whitespace.
whitespace :: Parser a -> Parser a
whitespace p = s *> p <* s
    where s = many(foldr (<|>) empty (map char ([' ', '\n', '\t'])))

-- Expected: [('a',Stream "")]
whitespaceEx :: [(Char, Stream)]
whitespaceEx = runParser (whitespace letter) (Stream "     a\t \n")

-- FP2.3
-- Contributors: Ion Tulei
-- Define a parser that parses one or more occurrences of `p` separated by `s`.
sep1 :: Parser a -> Parser b -> Parser [a]
sep1 p s = (:) <$> p <*> many (s *> p)

-- Expected: [("abcdef",Stream "")]
sep1Ex :: [(String, Stream)]
sep1Ex = runParser (sep1 letter (char ',')) (Stream "a,b,c,d,e,f")

-- Define a parser that parses zero or more occurrences of `p` optionally separated by `s`.
sep :: Parser a -> Parser b -> Parser [a]
sep p s = many (p <* optional s)

-- Expected: [("12345",Stream "")]
sepEx :: [(String, Stream)]
sepEx = runParser (sep dig (char ',')) (Stream "1,2,3,4,5")

-- Define a parser that optionally parses `p`, otherwise returns `x`.
option :: a -> Parser a -> Parser a
option x p = p <|> pure x

-- Expected: [('4',Stream "42a")]
optionEx :: [(Char, Stream)]
optionEx = runParser (option '4' letter) (Stream "42a")

-- FP2.4
-- Contributors: Dragoș Erhan
-- Define a parser that parses a specific string.
string :: String -> Parser String
string [] = pure ""
string (x:xs) = (:) <$> char x <*> string xs

-- Expected: [("abc",Stream "3")]
stringEx :: [(String, Stream)]
stringEx = runParser (string "abc") (Stream "abc3")

-- Define a parser that parses an identifier (whitespace followed by a letter and letters/digits).
identifier :: Parser String 
identifier = whitespace ((:) <$> letter <*> many (letter <|> dig))

-- Expected: [("fibonacci",Stream "x")]
identifierEx :: [(String, Stream)]
identifierEx = runParser identifier (Stream "   fibonacci x")

-- Define a parser that parses an integer (whitespace followed by one or more digits).
integer :: Parser Integer
integer = whitespace ((read) <$> some (dig))

-- Expected: [(234,Stream "a")]
integerEx :: [(Integer, Stream)]
integerEx = runParser integer (Stream "   234 a")

-- Define a parser that parses a specific symbol (string with surrounding whitespace).
symbol :: String -> Parser String
symbol = whitespace . string

-- Expected: [("+",Stream "2")]
symbolEx :: [(String, Stream)]
symbolEx = runParser (symbol "+") (Stream "   + 2")

-- Define a parser that parses something between parentheses.
parens :: Parser a -> Parser a
parens p = between (symbol "(") p (symbol ")")

-- Expected: [('a',Stream "sad")]
parensEx :: [(Char, Stream)]
parensEx = runParser (parens letter) (Stream "(a) sad")

-- Define a parser that parses something between braces.
braces :: Parser a -> Parser a
braces p = between (symbol "{") p (symbol "}")

-- Expected: [('1',Stream "")]
bracesEx :: [(Char, Stream)]
bracesEx = runParser (braces dig) (Stream "{1} sad")