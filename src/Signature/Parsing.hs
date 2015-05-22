module Signature.Parsing (parseSignature) where

import Text.ParserCombinators.Parsec hiding ((<|>))
import Text.ParserCombinators.Parsec.Error
import Control.Applicative
import Signature.Types

parseObj name obj = try $ do
  spaces
  try $ string name
  spaces
  x <- upper
  return $ obj x

-- | Parse filetypes from the signature
--
-- >>> parse value "documentation" "Fasta A"
-- Right (Fasta 'A')
value :: Parser SigObj
value = parseObj "Fasta" Fasta
    <|> parseObj "Fastq" Fastq
    <|> parseObj "Insert_size" InsertSize

-- | Parse list from the signature
--
-- >>> parse list "documentation" "[Fasta A]"
-- Right (SigList (Fasta 'A'))
--
list :: Parser SigObj
list = do
  char '['
  x <- value
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
  where values = value <|> list
        comma  = string ","



-- | Parse a biobox signature
--
-- >>> parse signature "documentation" "Fastq A -> Fastq A"
-- Right [[Fastq 'A'],[Fastq 'A']]
--
-- >>> parse signature "documentation" "Fastq A, Fastq B -> Fastq A"
-- Right [[Fastq 'A',Fastq 'B'],[Fastq 'A']]
--
-- >>> parse signature "documentation" "[Fastq A] -> Fastq A"
-- Right [[SigList (Fastq 'A')],[Fastq 'A']]
signature :: Parser ([[SigObj]])
signature = sepBy1 terms separator
  where separator = spaces >> string "->" >> spaces


-- | Parse a biobox signature
--
-- >>> parseSignature "Fastq A -> Fastq A"
-- Right ([Fastq 'A'],[Fastq 'A'])
--
-- >>> parseSignature "[Fastq A] -> Fastq A"
-- Right ([SigList (Fastq 'A')],[Fastq 'A'])
parseSignature :: String -> Either String ([SigObj], [SigObj])
parseSignature = (either err sig) . (parse signature header)
  where err x       = Left (show x)
        sig (x:y:_) = Right (x, y)
        header = "Error parsing biobox signature"
