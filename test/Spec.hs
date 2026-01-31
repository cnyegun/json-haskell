{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import JsonParser 
import Control.Applicative (Alternative(..))
import Test.Hspec
import Test.QuickCheck
import Data.Maybe (isNothing)
import Data.List (isPrefixOf)
import Data.Char

main :: IO ()
main = hspec $ do
    describe "parse" $ do

        describe "fmap" $ do 
            it "identity" $ do
                property $ \s s' -> 
                    parse (fmap id (string s)) s' == parse (id (string s)) s'

            it "composition" $ do
                property $ \(Fun _ f :: Fun String String) (Fun _ g :: Fun String String) s s' ->
                    parse (fmap (f . g) (string s)) s' == parse ((fmap f . fmap g) (string s)) s'

        describe "pure and <$>" $ do
            it "identity" $ do
                property $ \s s' -> 
                    parse (pure id <*> string s) s' == parse (string s) s'

            it "composition" $ do
                property $ \s' -> 
                    let 
                        u = pure reverse
                        v = pure (take 3)
                        w = string "123"
                    in parse (pure (.) <*> u <*> v <*> w) s' == parse (u <*> (v <*> w)) s'
                    
            it "homomorphism" $ do
                property $ \(Fun _ f :: Fun String String) s s' ->
                    parse ((pure f) <*> (pure s)) s' == parse (pure (f s)) s'

            it "interchange" $ do
                property $ \(someString::String) (y::String) s' -> 
                    let u = reverse <$ string someString
                    in parse (u <*> pure y) s' == parse (pure ($ y) <*> u) s'
        
        describe "<|> and empty" $ do
            it "left identity" $ do
                property $ \s s' ->
                    let p = string s
                    in parse (empty <|> p) s' == parse p s'

            it "right identity" $ do
                property $ \s s' ->
                    let p = string s
                    in parse (p <|> empty) s' == parse p s'
            
            it "associativity" $ do
                property $ \s1 s2 s3 s' ->
                    let a = string s1
                        b = string s2
                        c = string s3
                    in parse ((a <|> b) <|> c) s' == parse (a <|> (b <|> c)) s'

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
                property $ \rest -> 
                    let (_, rest') = span isSpace rest
                    in parse jsTrue ("true" ++ rest') == Just (JsBool True, rest')
            
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
                property $ \rest -> 
                    let (_, rest') = span isSpace rest
                    in parse jsFalse ("false" ++ rest') == Just (JsBool False, rest')
            
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
                property $ \rest -> 
                    let (_, rest') = span isSpace rest
                    in parse jsNull ("null" ++ rest') == Just (JsNull, rest')
            
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
            
        describe "ws" $ do
            it "parses the whitespace" $ do
                property $ \s -> 
                    parse ws s == Just (span isSpace s)

        describe "jsString" $ do
            it "parses the empty string" $ do
                parse jsString "\"\"" `shouldBe` Just (JsString "", "")

            it "parses a 'singleton' string" $ do
                parse jsString "\"c\"" `shouldBe` Just (JsString "c", "")

            it "parses any string" $ do
                parse jsString "\"name\"andmore" `shouldBe` Just (JsString "name", "andmore")
        
        describe "tok" $ do 
            it "parses and removes the spaces" $ do
                parse (tok (string "name")) "name  is  Luke" `shouldBe` Just ("name", "is  Luke")
            it "parses JsString and removes the spaces" $ do
                parse (tok jsString) "\"My name is Luke\"    } {}" `shouldBe` Just (JsString "My name is Luke", "} {}")

        describe "json" $ do
            it "parses with prefix spaces" $ do
                parse json "   \"@name\"   " == Just (JsString "@name", "")
        
        describe "jsNumber" $ do
            it "fails when there's no number" $ do
                property $ \input ->
                    input /= [] && isAlpha (head input) ==> isNothing $ parse jsNumber input

            it "parses non-negative number" $ do
                property $ \(n :: Int) text ->
                    text /= [] && isAlpha (head text) && n >= 0 ==> 
                        parse jsNumber (show n ++ text) == Just (JsNumber n, text)

            it "parses negative number" $ do
                property $ \(n :: Int) text ->
                    text /= [] && isAlpha (head text) && n < 0 ==> 
                        parse jsNumber (show n ++ text) == Just (JsNumber n, text)
            