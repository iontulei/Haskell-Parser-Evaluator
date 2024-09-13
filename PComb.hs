-- Add your names and student numbers to the following lines. Do not change anything else on these lines, since they are parsed.
-- Student 1: Dragoș Erhan (s2940124)
-- Student 2: Ion Tulei (s2928787)
-- Student 3: Third student (szzzzzzz)

module PComb where

import Control.Applicative
import Data.Char
import Test.QuickCheck

-- Stream of Chars - can be extended with the location for error handling.
data Stream = Stream [Char]
              deriving (Eq, Show)

-- FP1.1
-- Contributors: Ion Tulei
-- Define the Parser data type as a wrapper around a function that takes a Stream
-- and returns a list of possible parses, each with a result of type r and a remaining Stream.
data Parser r = P {
    runParser :: Stream -> [(r, Stream)]
}

-- FP1.2
-- Contributors: Dragoș Erhan
-- Implement Functor instance for Parser.
instance Functor Parser where
    fmap f p = P (\x -> [(f r, xs) | (r, xs) <- runParser p x])

-- FP1.3
-- Contributors: Ion Tulei
-- Define a parser that consumes a single specific character.
char :: Char -> Parser Char
char c = P p
    where
        p (Stream [])                 = [] 
        p (Stream (x:xs)) | c == x    = [(x, (Stream xs))]
                          | otherwise = []

-- Expected: [('1',Stream "23")]
charEx :: [(Char, Stream)]
charEx = runParser (char '1') (Stream "123")

-- FP1.4
-- Contributors: Dragoș Erhan
-- Define a parser that always fails.
failure :: Parser a
failure = P (\_ -> [])

-- Expected: []
failureEx :: [(a, Stream)]
failureEx = runParser failure (Stream "123")

-- FP1.5
-- Implement Applicative instance for Parser.
-- Contributors: Ion Tulei
instance Applicative Parser where
    pure a = P $ \xs -> [(a, xs)]
    p1 <*> p2 = P (\s -> [(r1 r2, s2) | (r1, s1) <- runParser p1 s, (r2, s2) <- runParser p2 s1])

-- FP1.6
-- Contributors: Dragoș Erhan
-- Implement Alternative instance for Parser.
instance Alternative Parser where
    empty = failure
    p1 <|> p2 = P (\s -> case runParser p1 s of
                              [] -> runParser p2 s
                              res -> res)