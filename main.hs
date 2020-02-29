{-# LANGUAGE BinaryLiterals #-}
import Data.Bits

h = 0b01
j= 0b1000

cards = [0b00000000,0b00000001,0b00000010,
 0b00000100,0b00000101,0b00000110,
 0b00001000,0b00001001,0b00001010,
 0b00010000,0b00010001,0b00010010,
 0b00010100,0b00010101,0b00010110,
 0b00011000,0b00011001,0b00011010,
 0b00100000,0b00100001,0b00100010,
 0b00100100,0b00100101,0b00100110,
 0b00101000,0b00101001,0b00101010,
 0b01000000,0b01000001,0b01000010,
 0b01000100,0b01000101,0b01000110,
 0b01001000,0b01001001,0b01001010,
 0b01010000,0b01010001,0b01010010,
 0b01010100,0b01010101,0b01010110,
 0b01011000,0b01011001,0b01011010,
 0b01100000,0b01100001,0b01100010,
 0b01100100,0b01100101,0b01100110,
 0b01101000,0b01101001,0b01101010,
 0b10000000,0b10000001,0b10000010,
 0b10000100,0b10000101,0b10000110,
 0b10001000,0b10001001,0b10001010,
 0b10010000,0b10010001,0b10010010,
 0b10010100,0b10010101,0b10010110,
 0b10011000,0b10011001,0b10011010,
 0b10100000,0b10100001,0b10100010,
 0b10100100,0b10100101,0b10100110,
 0b10101000,0b10101001,0b10101010]

aux x y | x == y = x
        | otherwise = 3 - (x+y)

(.?.) x y = (shift (aux (shift ( x .&. 192) (-6)) (shift ( y .&. 192) (-6))) 6) .|.(shift (aux (shift ( x .&. 48) (-4)) (shift ( y .&. 48) (-4))) 4) .|. (shift (aux (shift ( x .&. 12) (-2)) (shift ( y .&. 12) (-2))) 2) .|. (aux ( x .&. 3) ( y .&. 3))

checkSet (x, y, z) = (x .?. y) == z

pairs [] ys = []
pairs (x:xs) ys = (map (pA x) ys) ++ pairs xs ys
                 where pA x y = (x,y) 

everyComb [] ys = []
everyComb (x:xs) ys = (map (eCA x) ys) ++ everyComb xs ys
                     where eCA x (y,z) = (x,y,z)

noRep (x,y,z) = x /= y && x /= z && y /= z

isNotConm (x,y,z) (a,b,c) = not( (x == a && y == b && z == c) || (x == a && y == c && z == b) || (x == b && y == a && z == c) || (x == b && y == c && z == a) || (x == c && y == a && z == b) || (x == c && y == b && z == a) )

filterConm [] = []
filterConm (x:xs) = x : filterConm (filter (isNotConm x) xs)

set xs = filter checkSet ( filterConm ( filter noRep (everyComb xs (pairs xs xs))))

main = undefined
