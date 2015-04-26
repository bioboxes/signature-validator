module Signature.Types (SigObj(Fasta, Fastq, SigList)) where

data SigObj = Fasta Char
            | Fastq Char
            | SigList SigObj
  deriving (Eq, Show)

