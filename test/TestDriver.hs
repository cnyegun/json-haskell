module Main where

import System.IO
import Control.Monad 
import JsonParser 
import Data.List 

pretty :: Json -> String
pretty json = go 0 json
  where
    spaces n = replicate n ' '

    go n item = case item of
        JsNull       -> "null"
        JsBool True  -> "true"
        JsBool False -> "false"
        JsNumber num -> show num
        JsString str -> show str
        
        JsArray []   -> "[]"
        JsArray vals -> 
            "[\n" 
            ++ intercalate ",\n" (map (\v -> spaces (n + 2) ++ go (n + 2) v) vals) 
            ++ "\n" ++ spaces n ++ "]"

        JsObject []    -> "{}"
        JsObject pairs -> 
            "{\n" 
            ++ intercalate ",\n" (map (\(k, v) -> spaces (n + 2) ++ show k ++ ": " ++ go (n + 2) v) pairs) 
            ++ "\n" ++ spaces n ++ "}"

main :: IO ()
main = do
    putStrLn "Reading test.json..."
    
    content <- readFile "test/test.json"
    
    case parse jsValue content of
        Just (json, remainingInput) -> do
            putStrLn "Parsed successfully!"
            putStrLn (pretty json)
            
            if null remainingInput
                then putStrLn "\n(Perfect parse: No input left)"
                else putStrLn $ "\n(Warning: Unparsed input left: " ++ show (take 20 remainingInput) ++ "...)"
                
        Nothing -> do
            putStrLn "Failed to parse."
            putStrLn $ "Input starts with: " ++ take 50 content