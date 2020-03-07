{-# LANGUAGE BinaryLiterals #-}
import Data.Bits

h = 0b01
j= 0b1000

-- cards = [0b00000000,0b00000001,0b00000010,
--  0b00000100,0b00000101,0b00000110,
--  0b00001000,0b00001001,0b00001010,
--  0b00010000,0b00010001,0b00010010,
--  0b00010100,0b00010101,0b00010110,
--  0b00011000,0b00011001,0b00011010,
--  0b00100000,0b00100001,0b00100010,
--  0b00100100,0b00100101,0b00100110,
--  0b00101000,0b00101001,0b00101010,
--  0b01000000,0b01000001,0b01000010,
--  0b01000100,0b01000101,0b01000110,
--  0b01001000,0b01001001,0b01001010,
--  0b01010000,0b01010001,0b01010010,
--  0b01010100,0b01010101,0b01010110,
--  0b01011000,0b01011001,0b01011010,
--  0b01100000,0b01100001,0b01100010,
--  0b01100100,0b01100101,0b01100110,
--  0b01101000,0b01101001,0b01101010,
--  0b10000000,0b10000001,0b10000010,
--  0b10000100,0b10000101,0b10000110,
--  0b10001000,0b10001001,0b10001010,
--  0b10010000,0b10010001,0b10010010,
--  0b10010100,0b10010101,0b10010110,
--  0b10011000,0b10011001,0b10011010,
--  0b10100000,0b10100001,0b10100010,
--  0b10100100,0b10100101,0b10100110,
--  0b10101000,0b10101001,0b10101010]
cards:: [Int]
cards = [0,1,2,4,5,6,8,9,10,16,17,18,20,21,22,24,25,26,32,33,34,36,37,38,40,41,42,64,65,66,68,69,70,72,73,74,80,81,82,84,85,86,88,89,90,96,97,98,100,101,102,104,105,106,128,129,130,132,133,134,136,137,138,144,145,146,148,149,150,152,153,154,160,161,162,164,165,166,168,169,170]

-- * O(1)
aux x y | x == y = x
        | otherwise = 3 - (x+y)

-- * O(1)
(.?.) x y = (shift (aux (shift ( x .&. 192) (-6)) (shift ( y .&. 192) (-6))) 6) .|.(shift (aux (shift ( x .&. 48) (-4)) (shift ( y .&. 48) (-4))) 4) .|. (shift (aux (shift ( x .&. 12) ((-2))) (shift ( y .&. 12) ((-2)))) 2) .|. (aux ( x .&. 3) ( y .&. 3))

-- * checkset:: N -> M -> _     O(M*N)
checkSet _ [] = []
checkSet ys ((x, y):xs) | x == y = checkSet ys xs
                        | elem (x .?. y) ys = (x, y, (x .?. y)) : checkSet ys xs
                        | otherwise = checkSet ys xs

-- * O(N²)
pairs [] ys = []
pairs (x:xs) ys = (map ((,) x) ys) ++ pairs xs ys

-- * O(1)
isNotConm (x,y,z) (a,b,c) = not( (x == a && y == b && z == c) || (x == a && y == c && z == b) || (x == b && y == a && z == c) || (x == b && y == c && z == a) || (x == c && y == a && z == b) || (x == c && y == b && z == a) )

-- * O((N³+N²)/2)
filterConm [] = []
filterConm (x:xs) = x : filterConm (filter (isNotConm x) xs)

-- * O((N³+N²)/2)
set xs = filterConm (checkSet xs (pairs xs xs))
--Now with trees! :D

data Tree = E | L Int | N Tree Tree Tree deriving Show

-- The first param must be 3 to work properly 
-- * O(1)
insertElem (-1) x E = L x
insertElem a x E | (3 .&. (shift x (a*(-2)))) == 0 = N (insertElem (a-1) x E) E E 
                 | (3 .&. (shift x (a*(-2)))) == 1 = N E (insertElem (a-1) x E) E
                 | (3 .&. (shift x (a*(-2)))) == 2 = N E E (insertElem (a-1) x E) 
                 | otherwise = (E)
insertElem a x (N l m r) | (3 .&. (shift x (a*(-2)))) == 0 = N (insertElem (a-1) x l) m r 
                         | (3 .&. (shift x (a*(-2)))) == 1 = N l (insertElem (a-1) x m) r
                         | (3 .&. (shift x (a*(-2)))) == 2 = N l m (insertElem (a-1) x r) 
                         | otherwise = (N l m r)

-- The first param must be 3 to work properly 
-- * O(1)
findElem _ x E = False
findElem (-1) x (L a) = a == x
findElem a x (N l m r)  | (3 .&. (shift x (a*(-2)))) == 0 = findElem (a-1) x l 
                        | (3 .&. (shift x (a*(-2)))) == 1 = findElem (a-1) x m
                        | (3 .&. (shift x (a*(-2)))) == 2 = findElem (a-1) x r 
                        | otherwise = False

-- * O(N)
list2Tree xs = foldr (insertElem 3) (N E E E) xs

-- * O(N)
checkSetInTree t [] = []
checkSetInTree t ((x,y):xs) | x==y = checkSetInTree t xs
                            | (findElem 3 (x .?. y) t) = (x,y,(x .?. y)) : (checkSetInTree t xs)
                            | otherwise = (checkSetInTree t xs) 
                            -- | otherwise = if (findElem 3 (x .?. y) t) then (x,y,(x .?. y)) : (checkSetInTree t xs) else (checkSetInTree t xs)

-- * O((N³+N²)/2)
setTree xs = filterConm ( checkSetInTree (list2Tree xs) (pairs xs xs))

main = undefined