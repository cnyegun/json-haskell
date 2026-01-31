module Main where

import JsonParser 

import Test.Hspec
import Test.QuickCheck
import Data.Maybe (isNothing)
import Data.List (isPrefixOf)

main :: IO ()
main = hspec $ do
    describe "parse" $ do
        describe "char" $ do
            it "parsers matching characters" $ do
                property $ \c text -> parse (char c) (c:text) == Just (c, text)

            it "always fails given an empty string" $ do
                property $ \c -> isNothing (parse (char c) "")

            it "fails when meets a different char" $ do
                property $ \c d text -> c /= d ==> parse (char c) (d:text) `shouldBe` Nothing

        describe "string" $ do
            it "parses matching strings" $ do
                property $ \str1 str2 -> parse (string str1) (str1 ++ str2) `shouldBe` Just (str1, str2)

            it "always fails given an empty string" $ do
                property $ \s -> not (null s) ==> isNothing (parse (string s) "")
            
            it "always fails given a different string" $ do
                property $ \s rest -> not (s `isPrefixOf` rest) ==> isNothing (parse (string s) rest)

        describe "jsonTrue" $ do
            it "parses given `true` with any suffix" $ do
                property $ \rest -> parse jsTrue ("true" ++ rest) == Just (JsBool True, rest)
            
            it "always fails given otherwise" $ do
                property $ \s -> not ("true" `isPrefixOf` s) ==> isNothing (parse jsTrue s)
            
            it "always fails partial matches" $ do
                parse jsTrue "tru" `shouldBe` Nothing
                parse jsTrue "t" `shouldBe` Nothing
                parse jsTrue "TRUE" `shouldBe` Nothing
                parse jsTrue "True" `shouldBe` Nothing

            it "fails on empty string" $ do
                parse jsTrue "" `shouldBe` Nothing

        describe "jsonFalse" $ do
            it "parses given `false` with any suffix" $ do
                property $ \rest -> parse jsFalse ("false" ++ rest) == Just (JsBool False, rest)
            
            it "always fails given otherwise" $ do
                property $ \s -> not ("false" `isPrefixOf` s) ==> isNothing (parse jsFalse s)
            
            it "always fails partial matches" $ do
                parse jsFalse "fal" `shouldBe` Nothing
                parse jsFalse "f" `shouldBe` Nothing
                parse jsFalse "FALSE" `shouldBe` Nothing
                parse jsFalse "False" `shouldBe` Nothing

            it "fails on empty string" $ do
                parse jsFalse "" `shouldBe` Nothing

        describe "jsonNull" $ do
            it "parses given `null` with any suffix" $ do
                property $ \rest -> parse jsNull ("null" ++ rest) == Just (JsNull, rest)
            
            it "always fails given otherwise" $ do
                property $ \input -> not ("null" `isPrefixOf` input) ==> isNothing (parse jsNull input)
            
            it "always fails partial matches" $ do
                parse jsNull "nul" `shouldBe` Nothing
                parse jsNull "NULL" `shouldBe` Nothing
                parse jsNull "Null" `shouldBe` Nothing
                parse jsNull "nulL" `shouldBe` Nothing

            it "fails on empty string" $ do
                parse jsNull "" `shouldBe` Nothing

        describe "jsValue" $ do 
            it "parses true" $ do
                parse jsValue "true" `shouldBe` Just (JsBool True, "")
            
            it "parses false" $ do
                parse jsValue "false" `shouldBe` Just (JsBool False, "")
            
            it "parses null" $ do
                parse jsValue "null" `shouldBe` Just (JsNull, "")