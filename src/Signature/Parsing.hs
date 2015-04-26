module Signature.Parsing (parseSignature) where

import Text.ParserCombinators.Parsec hiding ((<|>))
import Control.Applicative
import Signature.Types

parseObj name obj = try $ do
  string name
  spaces
  x <- upper
  return $ obj x

-- | Parse filetypes from the signature
--
-- >>> parse parseFile "documentation" "Fasta A" 
-- Right (Fasta 'A')
--
-- >>> parse parseFile "documentation" "Fastq A" 
-- Right (Fastq 'A')
parseFile :: Parser SigObj
parseFile = parseObj "Fasta" Fasta
        <|> parseObj "Fastq" Fastq

-- | Parse list from the signature
--
-- >>> parse parseList "documentation" "[Fasta A]"
-- Right (SigList (Fasta 'A'))
--
parseList :: Parser SigObj
parseList = do
  char '['
  x <- parseFile
  char ']'
  return $ SigList x
  

-- | Parse the signature separator "->"
--
-- >>> parse parseSeparator "documentation" "->"
-- Right ()
parseSeparator :: Parser ()
parseSeparator = spaces >> (string "->") >> spaces


-- | Parse a biobox signature
--
-- >>> parse parseSignature "documentation" "Fastq A -> Fasta B"
-- Right [Fastq 'A',Fasta 'B']
--
-- >>> parse parseSignature "documentation" "[Fastq A] -> Fasta B"
-- Right [SigList (Fastq 'A'),Fasta 'B']
parseSignature = sepBy f parseSeparator
  where f = parseFile <|> parseList
