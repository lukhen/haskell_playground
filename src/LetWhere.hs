module LetWhere where

add2_1 :: Integer -> Integer
add2_1 x = plus2 x
  where plus2 = (+) 2

add2_2 :: Integer -> Integer
add2_2 x = let plus2 y = y + 2
          in plus2 x

