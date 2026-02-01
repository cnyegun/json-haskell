# A JSON parser written in Haskell

Well, there are like millions of Json parser in this world already, but this one is mine, and I like it. 

```haskell
data Json
    = JsBool Bool
    | JsNull
    | JsString String
    | JsNumber Int
    | JsArray [Json]
    | JsObject [(String, Json)]
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
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (Parser p) >>= f = Parser $ \input -> do
        (result, rest) <- p input
        let Parser p2 = f result
            in p2 rest
```