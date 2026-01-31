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
    , ws
    ) where
import Control.Applicative
import Data.Char

data Json
    = JsBool Bool
    | JsNull
    deriving (Show, Eq)

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) = Parser $ \text ->
        case p text of
            Nothing -> Nothing
            Just (x, rest) -> Just (f x, rest)

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \input -> Just (x, input)
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser pf) <*> (Parser p) = Parser $ \input -> do
        (f, rest) <- pf input
        (v, rest') <- p rest
        pure (f v, rest')

instance Alternative Parser where
    empty = Parser $ const Nothing
    (<|>) :: Parser a -> Parser a -> Parser a
    (Parser px) <|> (Parser py) = Parser $ \input ->
        case px input of 
            Just result -> Just result
            Nothing -> py input

satisfy :: (Char -> Bool) -> Parser Char
satisfy condition = Parser $ \case
        (x:xs) | condition x -> Just (x, xs)
        _ -> Nothing

char :: Char -> Parser Char
char c = satisfy (==c) 

string :: String -> Parser String
string "" = pure ""
string (c:cs) = (:) <$> char c <*> string cs

jsTrue :: Parser Json
jsTrue = JsBool True <$ string "true"

jsFalse :: Parser Json
jsFalse = JsBool False <$ string "false"

jsNull :: Parser Json
jsNull = JsNull <$ string "null"

jsValue :: Parser Json
jsValue = jsTrue <|> jsFalse <|> jsNull

ws :: Parser String
ws = many (satisfy isSpace)