module CaesarCipher where

import Data.Char

caesar :: Int -> String -> String
caesar _ "" = ""
caesar 0 xs = xs
caesar n (x:xs) = (chr $ 97 + i) : caesar n xs
  where p = ord x
        p' = (p + n)
        i = mod (p' - 97) 26

uncaesar :: Int -> String -> String
uncaesar n = caesar (26 - n)

