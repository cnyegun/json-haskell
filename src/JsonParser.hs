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
    , jsString
    , tok
    , json
    , jsNumber
    ) where
import Control.Applicative
import Data.Char

data Json
    = JsBool Bool
    | JsNull
    | JsString String
    | JsNumber Int
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

instance Monad Parser where
    (Parser p) >>= f = Parser $ \input -> do
        (result, rest) <- p input
        let Parser p2 = f result
            in p2 rest


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
jsTrue = JsBool True <$ tok (string "true")

jsFalse :: Parser Json
jsFalse = JsBool False <$ tok (string "false")

jsNull :: Parser Json
jsNull = JsNull <$ tok (string "null")

json :: Parser Json
json = ws *> jsValue

jsValue :: Parser Json
jsValue = jsTrue <|> jsFalse <|> jsNull <|> jsString <|> jsNumber

ws :: Parser String
ws = many (satisfy isSpace)

escapeMap :: Parser Char
escapeMap = ('"'  <$ char '"')   
        <|> ('\\' <$ char '\\')  
        <|> ('/'  <$ char '/')   
        <|> ('\b' <$ char 'b')   
        <|> ('\f' <$ char 'f')   
        <|> ('\n' <$ char 'n')   
        <|> ('\r' <$ char 'r')   
        <|> ('\t' <$ char 't')   

escaped :: Parser Char
escaped = char '\\' *> escapeMap

stringChar :: Parser Char
stringChar = escaped <|> normal
    where normal = satisfy (\c -> c /= '\\' && c /= '"')

jsString :: Parser Json
jsString = JsString <$> tok (char '"' *> many stringChar <* char '"')

tok :: Parser a -> Parser a
tok p = p <* ws

jsNumber :: Parser Json
jsNumber = tok $ do 
    s <- string "-" <|> pure ""
    d <- some (satisfy isDigit)
    pure $ JsNumber (read $ s ++ d)