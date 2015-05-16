module Signature.Parsing (parseSignature) where

import Text.ParserCombinators.Parsec hiding ((<|>))
import Text.ParserCombinators.Parsec.Error
import Control.Applicative
import Signature.Types

parseObj name obj = try $ do
  spaces
  string name
  spaces
  x <- upper
  return $ obj x

-- | Parse filetypes from the signature
--
-- >>> parse parseFile "documentation" "Fasta A"
-- Right (Fasta 'A')
--
-- >>> parse parseFile "documentation" " Fastq A "
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



-- | Parses the the left/right terms of a signature
--
-- >>> parse terms "documentation" "Fastq A"
-- Right [Fastq 'A']
--
-- >>> parse terms "documentation" "Fastq A, Fastq A"
-- Right [Fastq 'A',Fastq 'A']
--
-- >>> parse terms "documentation" "[Fastq A], Fastq A"
-- Right [SigList (Fastq 'A'),Fastq 'A']
terms :: Parser ([SigObj])
terms = sepBy values comma
  where values = parseFile <|> parseList
        comma  = string ","



-- | Parse a biobox signature
--
-- >>> parse signature "documentation" "Fastq A -> Fastq A"
-- Right [[Fastq 'A'],[Fastq 'A']]
--
-- >>> parse signature "documentation" "Fastq A, Fastq B -> Fastq A"
-- Right [[Fastq 'A',Fastq 'B'],[Fastq 'A']]
signature :: Parser ([[SigObj]])
signature = sepBy1 terms separator
  where separator = spaces >> string "->" >> spaces


-- | Parse a biobox signature
--
-- >>> parseSignature "Fastq A -> Fastq A"
-- Right ([Fastq 'A'],[Fastq 'A'])
parseSignature :: String -> Either String ([SigObj], [SigObj])
parseSignature = (either err sig) . (parse signature header)
  where err x       = Left (show x)
        sig (x:y:_) = Right (x, y)
        header = "Error parsing biobox signature"
