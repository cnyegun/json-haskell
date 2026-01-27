module Main where

data JsonValue 
    = JsonNull
    | JsonBool Bool
    | JsonNumber Integer
    | JsonString String
    | JsonArray [JsonValue]
    | JsonObject [(String, JsonValue)]

main :: IO ()
main = undefined