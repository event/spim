module VCard where

import VCommon

newtype VCard = VCard VCommon deriving Show

instance Read VCard where
    readsPrec _ str = VCard (read str) 