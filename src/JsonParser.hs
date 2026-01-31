{-# LANGUAGE LambdaCase #-}
module JsonParser 
    ( parse
    , char
    , string
    , jsTrue 
    , jsFalse
    , Json (..)

    ) where

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

char :: Char -> Parser Char
char c = Parser $ \case
    (x:xs) | c == x -> Just (c, xs)
           | otherwise -> Nothing
    [] -> Nothing

string :: String -> Parser String
string "" = Parser $ \input -> Just ("", input)
string (c:cs) = Parser $ \input -> do
    (_, rest) <- parse (char c) input
    (_, rest') <- parse (string cs) rest
    pure (c:cs, rest')

newtype Json
    = JsBool Bool
    deriving (Show, Eq)

jsTrue :: Parser Json
jsTrue = Parser $ \input -> do
    (_, rest) <- parse (string "true") input
    pure (JsBool True, rest)

jsFalse :: Parser Json
jsFalse = Parser $ \input -> do
    (_, rest) <- parse (string "false") input
    pure (JsBool False, rest)