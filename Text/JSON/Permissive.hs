-----------------------------------------------------------------------------
-- |
-- Module      :  Text.JSON.Permissive
-- Copyright   :  (c) Jonathan Kochems 2015
-- License     :  BSD-3
-- Maintainer  :  jonathan.kochems@gmail.com
-- Stability   :  experimental
-- Portability :  nonportable
--
-- This module extends Text.JSON to enable the decoding of strings containing literal JS objects.
-- In particular, it relaxes the restrictions that fields in JSON objects must be strings.
--
-- For example:
--
-- >  JSON conformant:                   literal JS object:
-- >  { "foo" : "bar" }                  { foo : "bar" }
--
-----------------------------------------------------------------------------
module Text.JSON.Permissive(decodePermissive, get_fields) where
import Text.JSON (Result(..))
import Text.JSON.Parsec ( runParser, try, CharParser(..), spaces, space, char, sepBy, manyTill, anyChar,
                          string, p_number, p_string, p_boolean, p_null, many, choice, noneOf, option, 
                          lookAhead, ParseError,  optionMaybe )
import Text.JSON.Types (JSValue(..), JSObject(..), toJSObject, toJSString, fromJSObject)
import Control.Monad(mzero)
import Data.Maybe (fromMaybe, isJust)
import Control.Applicative ((<$>))

 
{--------------------------------------------------------------------
  Decoding
--------------------------------------------------------------------}
-- | decodes a string encoding a JSON object in a relaxed fashion
--
-- > decode "{ foo : \"bar\" }"
-- > Error "Malformed JSON: expecting string: foo : \"b"
-- >
-- > decodePermissive "{ foo : \"bar\" }"      == Ok $ toJSObject [("foo", JSString $ toJSString "bar")]
-- > decodePermissive "{ \"foo\" : \"bar\" }"  == Ok $ toJSObject [("foo", JSString $ toJSString "bar")]
decodePermissive :: String -> Result (JSObject JSValue)
decodePermissive s = either (Error . show) 
                           (Ok    . toJSObject) 
                            $ runParser p_object () "stdin" s

{--------------------------------------------------------------------
  JSON Object Interaction
--------------------------------------------------------------------}
-- | returns the list of fields of a JSON Object
--
-- > do obj <- decodePermissive "{ foo : \"bar\", fooz : \"baz\" }"
-- >    return $ get_fields obj  == Ok ["foo", "fooz"]
--
-- > do obj <- decodePermissive "{ foo : \"bar\", fooz : \"baz\" }"
-- >    return $ get_field obj $ head $ get_fields obj  == Ok (Just $ JSString $ toJSString "bar" )
get_fields :: JSObject a -> [String]
get_fields = map fst . fromJSObject


{--------------------------------------------------------------------
  Helper Parsers
--------------------------------------------------------------------}
-- The main change is in p_object to relax the restrictions on 
-- fields.
p_object :: CharParser () [(String, JSValue)]
p_object = do _ <- spaces
              _  <- char '{'
              _ <- spaces
              rs <- option [] $ try $ entry `sepBy` string ","
              _ <- spaces
              _  <- char '}'
              return rs
    where entry = do _ <- spaces 
                     x   <- choice [try_wrapper p_string' "\"", many $ noneOf ":, }"]
                     _ <- spaces 
                     _ <- char ':'
                     _ <- spaces
                     val <- p_jvalue
                     _ <- spaces
                     return (x,val)

-- p_jvalue just hooks in the p_object parser (and also the modified p_array parser)                     
p_jvalue :: CharParser () JSValue
p_jvalue = choice [ (JSObject . toJSObject) <$> try_wrapper p_object "{", 
                    JSArray                 <$> try_wrapper p_array "[",
                    (JSString . toJSString) <$> try_wrapper p_string' "\"",
                    JSRational False        <$> try p_number,
                    JSBool                  <$> try p_boolean,
                    (\() -> JSNull)         <$> try p_null
                  ]

-- p_array just hooks into the p_jvalue parser
p_array :: CharParser () [JSValue]
p_array = do _  <- char '['
             rs <- entry  `sepBy` string ","
             _  <- char ']'
             return rs
    where entry = do _ <- spaces
                     res <- p_jvalue
                     _ <- spaces
                     return res

-- p_string' is a modified version of p_string that preserves leading whitespace. From the documention 
-- of Text.JSON it would appear this is undesired behaviour. However,  the quick check tests show that
-- Text.JSON.decode in fact does preserve leading whitespace in strings.
p_string' = do ws <- leadingWhiteSpace
               s  <- p_string
               return $ ws ++ s

leadingWhiteSpace = lookAhead $ try $ do _ <- string "\""
                                         many space 
                                          

-- Helper function to use in non-deterministic choice with lookAhead
try_wrapper :: CharParser () a -> String -> CharParser () a
try_wrapper p prefix = do try $ lookAhead (string prefix)
                          try p



