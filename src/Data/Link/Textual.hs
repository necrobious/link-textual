{-# LANGUAGE OverloadedStrings,FlexibleInstances #-}
module Data.Link.Textual where

import Data.Textual
import Data.Link
import Network.URI (URI)
import qualified Data.List                  as List
import qualified Data.Text                  as StrictText
import qualified Data.Text.Lazy             as LazyText
import qualified Data.Text.Encoding         as StrictTextEncoding
import qualified Data.Text.Lazy.Encoding    as LazyTextEncoding
import qualified Data.ByteString            as StrictByteString
import qualified Data.ByteString.Char8      as StrictChar8 
import qualified Data.ByteString.Lazy       as LazyByteString
import qualified Data.ByteString.Lazy.Char8 as LazyChar8

instance Textual Link where
  toLazyText (Link lvs) = 
    LazyText.intercalate  (","::LazyText.Text) (map toLazyText lvs)
  toText (Link lvs) = 
    StrictText.intercalate  (","::StrictText.Text) (map toText lvs)
  toString (Link lvs) = 
    List.intercalate  "," (map toString lvs)
  toUtf8BS (Link lvs) = 
    StrictByteString.intercalate  (","::StrictByteString.ByteString) (map toUtf8BS lvs)
  toUtf8LBS (Link lvs) = 
    LazyByteString.intercalate  (","::LazyByteString.ByteString) (map toUtf8LBS lvs)


instance Textual LinkValue where
  toLazyText (LinkValue uri lps) = 
    LazyText.intercalate (";"::LazyText.Text) $ (toLazyText uri) : (map toLazyText lps)
  toText (LinkValue uri lps) = 
    StrictText.intercalate (";"::StrictText.Text) $ (toText uri) : (map toText lps)
  toString (LinkValue uri lps) = 
    List.intercalate ";" $ (toString uri) : (map toString lps)
  toUtf8BS (LinkValue uri lps) = 
    StrictByteString.intercalate (";"::StrictByteString.ByteString) $ (toUtf8BS uri) : (map toUtf8BS lps)
  toUtf8LBS (LinkValue uri lps) = 
    LazyByteString.intercalate (";"::LazyByteString.ByteString) $ (toUtf8LBS uri) : (map toUtf8LBS lps)

instance Textual URI where
  toLazyText uri =
    LazyText.concat ["<"::LazyText.Text, LazyText.pack $ show uri, ">"::LazyText.Text]
  toText uri =
    StrictText.concat ["<"::StrictText.Text, StrictText.pack $ show uri, ">"::StrictText.Text]
  toString uri =
    List.concat ["<", show uri, ">"]
  toUtf8BS uri =
    StrictByteString.concat ["<"::StrictByteString.ByteString, StrictChar8.pack $ show uri, ">"::StrictByteString.ByteString]
  toUtf8LBS uri =
    LazyByteString.concat ["<"::LazyByteString.ByteString, LazyChar8.pack $ show uri, ">"::LazyByteString.ByteString]


instance Textual (String,String) where
  toLazyText (k,v) =
    LazyText.concat [LazyText.pack k, "=\""::LazyText.Text, LazyText.pack v, "\""::LazyText.Text]
  toText (k,v) =
    StrictText.concat [StrictText.pack k, "=\""::StrictText.Text, StrictText.pack v, "\""::StrictText.Text]
  toString (k,v) =
    List.concat [k, "=\"", v, "\""]
  toUtf8BS (k,v) =
    StrictByteString.concat [StrictChar8.pack k, "=\""::StrictByteString.ByteString, StrictChar8.pack v, "\""::StrictByteString.ByteString]
  toUtf8LBS (k,v) =
    LazyByteString.concat [LazyChar8.pack k, "=\""::LazyByteString.ByteString, LazyChar8.pack v, "\""::LazyByteString.ByteString]
   
