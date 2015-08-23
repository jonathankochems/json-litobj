{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

{-Imports for testing-}
import Test.Hspec
import Test.QuickCheck

{-Basic libraries-}
import Data.Char(isPrint)
--import qualified Control.Exception as Except
import Data.Either
import Control.Applicative ((<$>))

import Text.JSON (encode, decode, resultToEither)
import Text.JSON.Types (toJSObject, toJSString, JSValue(..), JSObject, JSString)

{-Modules to test-}
import Text.JSON.Permissive


main :: IO ()
main = textJsonPermissive

textJsonPermissive :: IO ()
textJsonPermissive = hspec $
  describe "Text.JSON.Permissive" $ do
    it "should parse JSON objects with relaxed field syntax" $ do
        let nonstrict_json_string = "{ foo : \"bar\" }"
            json_object           = toJSObject [("foo", JSString $ toJSString "bar")]
        either (\_ -> error "fail") 
                id 
                (resultToEither $ decodePermissive nonstrict_json_string) `shouldBe` json_object
    it "should parse JSON objects with relaxed and normal field syntax" $ do
        let mixed_json_string = "{ foo : \"bar\", \"fooz\" : \"baz\" }"
            json_object           = toJSObject [("foo", JSString $ toJSString "bar"), ("fooz", JSString $ toJSString "baz")]
        either (\_ -> error "fail") 
                id 
                (resultToEither $ decodePermissive mixed_json_string) `shouldBe` json_object
    it "should parse JSON objects produced by Text.JSON.encode" $ do
        let json_object           = toJSObject [("foo", JSString $ toJSString "bar"), ("fooz", JSString $ toJSString "baz")]
            json_object_string    = encode json_object
        either (\_ -> error "fail") 
                id 
                (resultToEither $ decodePermissive json_object_string) `shouldBe` json_object
    it "should parse *all* JSON objects produced by Text.JSON.encode" $
       property $ 
           forAll jsonObject $ 
            \json_object -> 
                either (\s -> error $ "fail non-strict: " ++ show s ) 
                       id 
                       (resultToEither . decodePermissive $ encode json_object) `shouldBe` 
                either (\s -> error $ "fail conformant: " ++ s ) 
                       id 
                       (resultToEither . decode $ encode json_object) 
    it "should give access to all available fields of a JSON object" $ do
      let fields = either (\s -> error $ "failed to parse: " ++ show s ) 
                          id 
                          (resultToEither $ get_fields <$> decodePermissive "{ foo : \"bar\", fooz : \"baz\" }" )
      fields `shouldBe` ["foo", "fooz"]


jsonObject :: Gen (JSObject JSValue)
jsonObject = do
   depth_bound <- choose(0,4)
   json_object_with_depth depth_bound

jsonValueWithDepth :: Int -> Gen JSValue
jsonValueWithDepth d = oneof [ fmap JSString json_string,
                               JSObject <$> json_object_with_depth d,
                               JSArray  <$> json_array_with_depth d,
                               elements [JSNull],
                               fmap JSBool arbitrary,
                               fmap (uncurry JSRational) arbitrary 
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
