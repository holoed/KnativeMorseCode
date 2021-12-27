module MorseCodeTable where

decode :: [Char] -> Maybe Char
decode ".-"   = Just 'A'
decode "-..." = Just 'B'
decode "-.-." = Just 'C'
decode "-.."  = Just 'D'
decode "."    = Just 'E'
decode "..-." = Just 'F'
decode "--."  = Just 'G'
decode _ = Nothing