module VigenereCipher where

import Data.Char
import Data.List

mapKeyword :: String -> String -> String
mapKeyword "" kw = ""
mapKeyword s kw = go s (cycle kw) 0
  where
    go "" _ _ = ""
    go s' kwCycle offset =
      take numChars kwCycle ++
      replicate numSpaces ' ' ++
      go (drop totalChars s') (drop numChars kwCycle) (offset + numChars)
      where
        numChars = (length $ takeWhile (/= ' ') s')
        numSpaces = (length $ takeWhile (== ' ') $ drop numChars s')
        totalChars = numChars + numSpaces

vigenere :: String -> String -> String
vigenere "" kw = ""
vigenere s kw = map vigenereEncode stringCipherPairs
  where
    cipher = mapKeyword s kw
    stringCipherPairs = zip s cipher
    vigenereEncode (wordChar, cipherChar) =
      if wordChar == ' '
        then ' '
        else let delta = elemIndex cipherChar ['a' .. 'z']
             in case delta of
                  Just i -> chr $ 97 + mod (ord wordChar + i - 97) 26
                  _ -> '?'
