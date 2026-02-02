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
                        parse jsNumber (show n ++ text) == Just (JsNumber (fromIntegral n), text)

            it "parses negative number" $ do
                property $ \(n :: Int) text ->
                    text /= [] && isAlpha (head text) && n < 0 ==> 
                        parse jsNumber (show n ++ text) == Just (JsNumber (fromIntegral n), text)

            it "parses floating point numbers" $ do
                parse jsNumber "3.14" `shouldBe` Just (JsNumber 3.14, "")
                parse jsNumber "-2.5" `shouldBe` Just (JsNumber (-2.5), "")
                parse jsNumber "0.123" `shouldBe` Just (JsNumber 0.123, "")

            it "parses numbers with exponents" $ do
                parse jsNumber "1e10" `shouldBe` Just (JsNumber 1e10, "")
                parse jsNumber "2.5E-3" `shouldBe` Just (JsNumber 2.5e-3, "")
                parse jsNumber "1.2e+5" `shouldBe` Just (JsNumber 1.2e5, "")
        
        describe "jsArray" $ do
            it "fails when input is not an array" $ do
                parse jsArray "hihih_\rdf23415" `shouldBe` Nothing
            
            it "parses empty array" $ do
                parse jsArray "[]" `shouldBe` Just (JsArray [], "")

            it "parses normal array" $ do
                parse jsArray "[1,  66, \"hihi\", false   ]" `shouldBe` Just (JsArray [JsNumber 1.0, JsNumber 66.0, JsString "hihi", JsBool False], "")

            it "parses nested arrays" $ do
                parse jsArray "[1, [2, 3]]" `shouldBe` Just (JsArray [JsNumber 1.0, JsArray [JsNumber 2.0, JsNumber 3.0]], "")
        
        describe "sepBy" $ do
            let parser = sepBy (char 'X') (char ',' <* ws)
            it "returns an empty list if there is nothing to parse" $ do
                parse parser "" `shouldBe` Just ([], "")
                parse parser "SomethingThatDoesntMatch" `shouldBe` Just ([], "SomethingThatDoesntMatch")
        
            it "parses single element" $ do
                parse parser "X" `shouldBe` Just ("X", "")

            it "parses simple json array (inside)" $ do
                parse parser "X,X,X,X" `shouldBe` Just ("XXXX", "")
                parse parser "X,   X, X, Invalid" `shouldBe` Just ("XXX", ", Invalid")

            it "parses nested array" $ do
                parse jsArray "[ [1] , [2] ]" `shouldBe` Just (JsArray [JsArray [JsNumber 1.0], JsArray [JsNumber 2.0]], "")
            
        
        describe "jsObject" $ do
            it "parses an empty object" $ do
                parse jsObject "{}" `shouldBe` Just (JsObject [], "")
                parse jsObject "{     }" `shouldBe` Just (JsObject [], "")
            
            it "parses a single pair" $ do
                parse jsObject "{\"foo\":   123}" `shouldBe` Just (JsObject [("foo", JsNumber 123.0)], "")

            it "parses many pairs" $ do
                parse jsObject "{\"foo\": 123, \"bar\" : \"hi\"}" `shouldBe` Just (JsObject [("foo", JsNumber 123.0), ("bar", JsString "hi")], "")

            it "parses nested JsObject as well!" $ do
                parse jsObject "{\"foo\": { \"bar\": 123 }}" `shouldBe` Just (JsObject [("foo", JsObject [("bar", JsNumber 123.0)])], "")
        
        describe "jsPair" $ do
            it "return empty list if nothing to parse" $ do
                parse jsPair "" `shouldBe` Nothing
            
            it "parse an normal key value" $ do
                parse jsPair "\"foo\":\"bar\"" `shouldBe` Just (("foo", JsString "bar"), "")
                parse jsPair "\"age\":32" `shouldBe` Just (("age", JsNumber 32.0), "")
                parse jsPair "\"age\"   :   32   " `shouldBe` Just (("age", JsNumber 32.0), "")

        describe "stringLiteral" $ do
            it "fails on empty string" $ do
                parse stringLiteral "" `shouldBe` Nothing
            it "parses an empty string" $ do
                parse stringLiteral "\"\"" `shouldBe` Just ("", "")
            it "parses a single character" $ do
                parse stringLiteral "\"h\"" `shouldBe` Just ("h", "")
            it "parses a normal string" $ do
                parse stringLiteral "\"nameAgeGroup\"" `shouldBe` Just ("nameAgeGroup", "")
            