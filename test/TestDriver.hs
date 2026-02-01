module Main where

import System.IO
import Control.Monad (when)
import JsonParser (parse, jsValue, Json) 

main :: IO ()
main = do
    putStrLn "Reading test.json..."
    
    content <- readFile "test/test.json"
    
    case parse jsValue content of
        Just (json, remainingInput) -> do
            putStrLn "--------------------------------"
            putStrLn "SUCCESS! Parsed Structure:"
            putStrLn "--------------------------------"
            print json 
            
            if null remainingInput
                then putStrLn "\n(Perfect parse: No input left)"
                else putStrLn $ "\n(Warning: Unparsed input left: " ++ show (take 20 remainingInput) ++ "...)"
                
        Nothing -> do
            putStrLn "--------------------------------"
            putStrLn "FAILURE: Could not parse."
            putStrLn "--------------------------------"
            putStrLn $ "Input starts with: " ++ take 50 content