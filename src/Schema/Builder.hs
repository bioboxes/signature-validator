{-# LANGUAGE OverloadedStrings #-} 

module Schema.Builder (build) where

import Data.Yaml
import qualified Data.ByteString.Char8 as B
  
arguments = object [
    "type"     .= String "array"
  , "minItems" .= Number 1
  , "maxItems" .= Number 2
  , "items"    .= object []
  ]

document = object [
    "$schema"  .= String "http://json-schema.org/draft-04/schema#"
  , "type"     .= String "object"
  , "additionalProperties" .= False
  , "properties" .= object [
          "version"   .= object ["type" .= String "string", "pattern" .= String "^0.9.\\d+$"]
        , "arguments" .= arguments 
      ]
  ]

build :: String
build = B.unpack . encode $ document
