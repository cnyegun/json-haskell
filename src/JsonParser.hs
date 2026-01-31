{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
module JsonParser
    ( parse
    , char
    , string
    , jsTrue
    , jsFalse
    , Json (..)
    , jsNull
    , jsValue
    ) where
import Control.Applicative

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \text ->
        case p text of
            Nothing -> Nothing
            Just (x, rest) -> Just (f x, rest)

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \input -> Just (x, input)
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser pf) <*> (Parser pv) = Parser $ \input ->
        case pf input of
            Nothing -> Nothing
            Just (f, rest) -> case pv rest of
                Nothing -> Nothing
                Just (val, rest') -> Just (f val, rest')

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser px) <|> (Parser py) = Parser $ \input ->
        case px input of 
            Just result -> Just result
            Nothing -> py input

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

data Json
    = JsBool Bool
    | JsNull
    deriving (Show, Eq)

jsTrue :: Parser Json
jsTrue = Parser $ \input -> do
    (_, rest) <- parse (string "true") input
    pure (JsBool True, rest)

jsFalse :: Parser Json
jsFalse = Parser $ \input -> do
    (_, rest) <- parse (string "false") input
    pure (JsBool False, rest)

jsNull :: Parser Json
jsNull = Parser $ \input -> do
    (_, rest) <- parse (string "null") input
    pure (JsNull, rest)

jsValue :: Parser Json
jsValue = jsTrue <|> jsFalse <|> jsNull