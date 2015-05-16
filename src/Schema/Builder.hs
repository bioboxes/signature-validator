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

schema_entry x = object [
    "required" .= array [ String "fastq" ]
  , "additionalProperties" .= False
  , "type" .= String "object"
  , "properties" .= object [
      x .= object ["$ref" .= String "#/definitions/value"]
    ]
  ]

definitions = object [
    "value" .= object [ 
        "type" .= String "object"
      , "additionalProperties" .= False
      , "required" .= array [String "id", String "value", String "type"]
      , "properties" .= object [
          "id"    .= object ["type" .= String "string"]
        , "value" .= object ["type" .= String "string"]
        , "type"  .= object ["type" .= String "string"]
      ]
    ]
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

term (Fastq _)   = schema_entry "fastq"
term (Fasta _)   = schema_entry "fasta"

build :: [SigObj] -> Either String String
build x = Right . B.unpack . encode $ document $ map term x
