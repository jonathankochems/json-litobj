{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

{-Imports for testing-}
import Test.Hspec
import Test.QuickCheck

{-Basic libraries-}
import Data.Char(isPrint)
--import qualified Control.Exception as Except
import Data.Either

import Text.JSON (encode, decode, resultToEither)
import Text.JSON.Types (toJSObject, toJSString, JSValue(..), JSObject, JSString)

{-Modules to test-}
import Text.JSON.NonStrict


main :: IO ()
main = textJsonNonstrict

textJsonNonstrict :: IO ()
textJsonNonstrict = hspec $
  describe "Text.JSON.NonStrict" $ do
    it "should parse JSON objects with relaxed field syntax" $ do
        let nonstrict_json_string = "{ foo : \"bar\" }"
            json_object           = toJSObject [("foo", JSString $ toJSString "bar")]
        either (\_ -> error "fail") 
                id 
                (resultToEither $ decodeNonStrict nonstrict_json_string) `shouldBe` json_object
    it "should parse JSON objects with relaxed and normal field syntax" $ do
        let mixed_json_string = "{ foo : \"bar\", \"fooz\" : \"baz\" }"
            json_object           = toJSObject [("foo", JSString $ toJSString "bar"), ("fooz", JSString $ toJSString "baz")]
        either (\_ -> error "fail") 
                id 
                (resultToEither $ decodeNonStrict mixed_json_string) `shouldBe` json_object
    it "should parse JSON objects produced by Text.JSON.encode" $ do
        let json_object           = toJSObject [("foo", JSString $ toJSString "bar"), ("fooz", JSString $ toJSString "baz")]
            json_object_string    = encode json_object
        either (\_ -> error "fail") 
                id 
                (resultToEither $ decodeNonStrict json_object_string) `shouldBe` json_object
    it "should parse *all* JSON objects produced by Text.JSON.encode" $
       property $ 
           forAll jsonObject $ 
            \json_object -> 
                either (\s -> error $ "fail non-strict: " ++ show s ) 
                       id 
                       (resultToEither . decodeNonStrict $ encode json_object) `shouldBe` 
                either (\s -> error $ "fail conformant: " ++ s ) 
                       id 
                       (resultToEither . decode $ encode json_object) 

jsonObject :: Gen (JSObject JSValue)
jsonObject = do
   depth_bound <- choose(0,4)
   json_object_with_depth depth_bound

jsonValueWithDepth :: Int -> Gen JSValue
jsonValueWithDepth d = oneof [ fmap JSString $ json_string,
                               fmap JSObject $ json_object_with_depth d,
                               fmap JSArray  $ json_array_with_depth d,
                               elements $ [JSNull],
                               fmap JSBool $ arbitrary,
                               fmap (\(x,y) -> JSRational x y) $ arbitrary 
                             ]

json_string :: Gen JSString
json_string = do
    l <- choose(0,10)
    fmap toJSString $ suchThat (vector l) $ all isPrint

json_object_with_depth :: Int -> Gen (JSObject JSValue)
json_object_with_depth d = do
    num_fields <- choose(0,4)
    strings <- vector num_fields
    values  <- vectorOf num_fields $ jsonValueWithDepth (d-1)
    return $ toJSObject $ zip strings values

json_array_with_depth :: Int -> Gen [JSValue]
json_array_with_depth d = do
    l <- choose(0,4)
    vectorOf l $ jsonValueWithDepth (d-1)
