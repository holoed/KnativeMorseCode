module MorseCodeTable where

decode :: [Char] -> Maybe Char
decode ".-"   = Just 'A'
decode "-..." = Just 'B'
decode "-.-." = Just 'C'
decode "-.."  = Just 'D'
decode "."    = Just 'E'
decode "..-." = Just 'F'
decode "--."  = Just 'G'
decode "...." = Just 'H'
decode ".."   = Just 'I'
decode ".---" = Just 'J'
decode "-.-"  = Just 'K'
decode ".-.." = Just 'L'
decode "--"   = Just 'M'
decode _ = Nothing