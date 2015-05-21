{-# LANGUAGE OverloadedStrings #-}

module Schema.Builder (build) where

import Signature.Types
import Data.Yaml

import qualified Data.ByteString.Char8  as B

schema_array xs = object [
    "type"            .= String "array"
  , "additionalItems" .= False
  , "items"           .= array xs
  ]

schema_entry name value = object [
    "required" .= array [ String name ]
  , "additionalProperties" .= False
  , "type" .= String "object"
  , "properties" .= object [
      name .= object ["$ref" .= String value]
    ]
  ]

definition value = object [
      "type" .= String "object"
    , "additionalProperties" .= False
    , "required" .= array [String "id", String "value", String "type"]
    , "properties" .= object [
        "id"    .= object ["type" .= String "string"]
      , "value" .= object ["type" .= String value]
      , "type"  .= object ["type" .= String "string"]
    ]
  ]

definitions = object [
    "str_var" .= definition "string"
  , "num_var" .= definition "number"
  ]

document terms = object [
    "$schema"  .= String "http://json-schema.org/draft-04/schema#"
  , "type"     .= String "object"
  , "additionalProperties" .= False
  , "required" .= [String "arguments", String "version"]
  , "definitions" .= definitions
  , "properties" .= object [
          "version"   .= object ["type" .= String "string", "pattern" .= String "^0.9.\\d+$"]
        , "arguments" .= schema_array terms
      ]
  ]

term (SigList x) = schema_array [term x]
term (Fastq _)      = schema_entry "fastq" "#/definitions/str_var"
term (Fasta _)      = schema_entry "fasta" "#/definitions/str_var"
term (InsertSize _) = schema_entry "insert_size" "#/definitions/num_var"

build :: [SigObj] -> Either String String
build x = Right . B.unpack . encode $ document $ map term x
