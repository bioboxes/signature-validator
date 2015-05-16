{-# LANGUAGE OverloadedStrings #-} 

module Schema.Builder (build) where

import Signature.Types
import Data.Yaml
import qualified Data.ByteString.Char8 as B
  
arguments = object [
    "type"     .= String "array"
  , "additionalItems" .= False
  , "items"    .= array [
      object [
          "required" .= array [ String "fastq" ]
        , "additionalProperties" .= False
        , "type" .= String "object"
        , "properties" .= object [
            "fastq" .= object ["$ref" .= String "#/definitions/value"]
        ]
      ]
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

document = object [
    "$schema"  .= String "http://json-schema.org/draft-04/schema#"
  , "type"     .= String "object"
  , "additionalProperties" .= False
  , "properties" .= object [
          "version"   .= object ["type" .= String "string", "pattern" .= String "^0.9.\\d+$"]
        , "arguments" .= arguments 
      ]
  , "required" .= [String "arguments", String "version"]
  , "definitions" .= definitions
  ]

build :: [SigObj] -> Either String String
build _ = Right . B.unpack . encode $ document
