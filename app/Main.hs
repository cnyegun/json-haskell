module Main where

import JsonParser (parse, json, Json(..))
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> parseFile filePath
        []         -> parseStdin
        _          -> printUsage >> exitFailure

parseFile :: FilePath -> IO ()
parseFile filePath = do
    contents <- readFile filePath
    processJson contents

parseStdin :: IO ()
parseStdin = do
    contents <- getContents
    processJson contents

processJson :: String -> IO ()
processJson input =
    case parse json input of
        Just (result, remaining) -> do
            putStrLn "Parsed successfully!"
            putStrLn ""
            putStrLn "Result:"
            prettyPrint 0 result
            if null remaining
                then putStrLn "\n(No remaining input)"
                else putStrLn $ "\nRemaining input: " ++ show remaining
        Nothing -> do
            putStrLn "Failed to parse JSON input"
            exitFailure

prettyPrint :: Int -> Json -> IO ()
prettyPrint indent js = putStr $ prettyShow indent js

prettyShow :: Int -> Json -> String
prettyShow _ (JsBool True)  = "true"
prettyShow _ (JsBool False) = "false"
prettyShow _ JsNull         = "null"
prettyShow _ (JsString s)   = show s
prettyShow _ (JsNumber n)
    | isInt n   = show (round n :: Integer)
    | otherwise = show n
  where isInt x = x == fromIntegral (round x :: Integer)
prettyShow _ (JsArray []) = "[]"
prettyShow indent (JsArray xs) = 
    "[\n" ++ items ++ spaces indent ++ "]"
  where
    spaces n = replicate (n * 2) ' '
    items = unlines [spaces (indent + 1) ++ prettyShow (indent + 1) x ++ "," | x <- init xs]
         ++ spaces (indent + 1) ++ prettyShow (indent + 1) (last xs) ++ "\n"
prettyShow _ (JsObject []) = "{}"
prettyShow indent (JsObject kvs) =
    "{\n" ++ pairs ++ spaces indent ++ "}"
  where
    spaces n = replicate (n * 2) ' '
    showPair (k, v) = show k ++ ": " ++ prettyShow (indent + 1) v
    pairs = unlines [spaces (indent + 1) ++ showPair kv ++ "," | kv <- init kvs]
         ++ spaces (indent + 1) ++ showPair (last kvs) ++ "\n"

printUsage :: IO ()
printUsage = do
    putStrLn "JSON Parser - A simple JSON parser written in Haskell"
    putStrLn ""
    putStrLn "Usage:"
    putStrLn "  json-haskell <file>    Parse JSON from a file"
    putStrLn "  json-haskell           Parse JSON from stdin"
    putStrLn ""
    putStrLn "Examples:"
    putStrLn "  json-haskell test.json"
    putStrLn "  echo '{\"key\": \"value\"}' | json-haskell"
