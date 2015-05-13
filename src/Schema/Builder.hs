{-# LANGUAGE OverloadedStrings #-} 

module Schema.Builder (build) where

import Signature.Types
import Data.Yaml
import qualified Data.ByteString.Char8 as B
  
arguments = object [
    "type"     .= String "array"
  , "minItems" .= Number 1
  , "maxItems" .= Number 1
  , "items"    .= object [
      "oneOf" .= array [
        object ["$ref" .= String "#/definitions/fastq"]
      ]
    ]
  ]

definitions = object [
    "fastq" .= object [
        "type" .= String "object"
      , "additionalProperties" .= False
      , "required" .= [ String "fastq" ]
      , "properties" .= object [
        "$ref" .= String "#/definitions/value"
      ]
    ]
  , "value" .= object [ 
        "type" .= String "object"
      , "additionalProperties" .= False
      , "required" .= [ String "id", String "value", String "type"]
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

build :: SigObj -> Either String String
build _ = Right . B.unpack . encode $ document
