module Signature.Types (SigObj(Fasta, Fastq, InsertSize, SigList)) where

data SigObj = Fasta Char
            | Fastq Char
            | InsertSize Char
            | SigList SigObj
  deriving (Eq, Show)

