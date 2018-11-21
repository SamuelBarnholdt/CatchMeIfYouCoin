module SHA3_256 where

import Data.Bits
import Cryptography
import Data.List (sort)
import Data.Array
import Data.Char (ord)

{- Note Generally examples are not given, since the states are so big. Check
   the documentation for a better explanation. Niether are types except fo in
   the main functions.
-}



{-================================ Squeezeing ================================-}
{-============================================================================-}

{- sha3_256 decimal
   Hashing for integers.
   PRE: No longer than 1344 bits.
   RETURNS: The hash of a integer as a decimal number.
   EXAMPLES:-
-}
sha3_256 :: Integer -> Integer
sha3_256 a = btoDec . take 256 . concat . elems $ (sha3 0 (padding a))
-- test0 = padding concat `mod` 1344 should be 0

{- sha3_256S string
   Hashing for strings.
   PRE: Binary representation not longer than 1344 bits.
   RETURNS: The hash of a string as a decimal number
   EXAMPLES:-
-}
sha3_256S :: String -> Integer
sha3_256S a = sha3_256 $ (fromIntegral (sum $ map ord a ) :: Integer)

sha3 24 a = a
sha3 n a = sha3 (n+1) (keccak n a)

-- Runs the steps of the alorithm.
keccak n a = iota n . chi . rhoPi . theta $ a

-- Converts from binary to decimal.
btoDec :: [Integer]->Integer
btoDec xs = toDec (reverse xs) 0

toDec :: [Integer] -> Int -> Integer
toDec [] _ = 0
toDec (x:xs) n = x*2^n + toDec xs (n+1)


{-============================== Pre processing ==============================-}
{-============================================================================-}

{- preProcessing input
   Pre processing for keccak.
   PRE: True
   RETURNS: The integer as a binary number represented as a list
            with added padding.
-}
preProcessing :: Integer -> [Integer]
preProcessing input
    | padLength >= 1 && padLength <= 6 = binInp ++ p ++ [1] ++ (take (1344 - padLength) $ repeat 0) ++ [1] ++ (take 256 $ repeat 0)
    | padLength > 6 = binInp ++ p ++ [1] ++ (take (padLength - 7) $ repeat 0) ++ [1] ++ (take 256 $ repeat 0)
    where padLength  = r - ((length binInp) `mod` r)
          r = 1344
          p = [1,1,1,0,1]
          binInp = binary input

{-pre binaryStr
  Splits a binary string into lanes of 64 bits.
  PRE: A list of ones and zeros, must be a multiple of 64.-
  RETURNS: A list of lists with each containing a 64 bit
           binary number.
-}
pre :: [Integer] -> [[Integer]]
pre [] = []
pre binaryStr = take 64 binaryStr : pre (drop 64 binaryStr)

{- padding initV
   Pre processing for the keccak algorithm.
   PRE: True
   RETURNS: A 1600 bit binary string split into
            64 bit lanes.
-}
padding initV = listArray ((0,0), (4,4)) (pre $ preProcessing initV)


{-================================ Absorbing  ================================-}
{-============================================================================-}


{- laneXor bits n
   Performs xor operation between lanes column wise.
   PRE: Size of array 5x5x64. n=0.
   RETURNS: A list with 5 lanes.
-}
laneXor _ 5 = []
laneXor bits n = (zipWith xor (bits!(n,4)) . zipWith xor (bits!(n,3)) .
                zipWith xor (bits!(n,2)) . zipWith xor (bits!(n,1))
                    $ (bits!(n,0))) : laneXor bits (n+1)

{- cList bits n
   Rotates one lane and xors it with another.
   PRE: Size of array 5x5x64. n=0.
   RETURNS: A list with 5 lanes.
-}
cList _ 5 = []
cList bits n = zipWith xor (c!((n-1) `mod` 5))
                (rotateLane (c!((n+1) `mod` 5)) 1) : cList bits (n+1)
    where c = listArray (0,4) (laneXor bits 0)

{- rotateLane lane steps
   Rotates each element of a lane an arbitary number of steps.
   PRE: length lane = 64.
   RETURNS: The lane rotated an arbitary number of steps.
   EXAMPLES: rotateLane [1..5] 3 = [3,4,5,1,2] (Note that the function only works
             properly when the list is 64 bits long. This is only an example to
             show the principle.)
-}
rotateLane lane steps  = map snd . sort $ rotatel lane steps 0 64

{- rotatel listOfLanes steps n
   Calculates the new position of each element.
   PRE: length lane = 64. n = 0.
   RETURNS: A list consisting of tuples with new position first and the value
            second.
-}
rotatel [] _ _ p = []
rotatel (x:lane) steps n p = ((n+steps) `mod` p ,x) : rotatel lane steps (n+1) p

{-theta bits
  Performs various operations on the lanes.
  PRE: The dimensions of bits must be 5x5x64.
  RETURNS: A array with the same size as the input.
-}
theta :: (Ord c, Num a, Num b1, Num i, Num b2, Ix a, Ix b1, Ix i, Ix b2, Bits c) => Array (i, b2) [c] -> Array (a, b1) [c]
theta bits = listArray ((0,0),(4,4)) $ zipWith (zipWith xor)
            (elems bits) (map (\(x,_)-> d!x) (indices bits))
    where d = listArray (0,4) (cList bits 0)


{- rhoPi bits
   Rotates each lane a predefined number of steps and places the result in
   a new array at defined positions.
   PRE: size bits = 5x5x64
   RETURNS: A Array with the same size as the input, index from (0,0) to (4,4)
-}
rhoPi bits =
    let
        rotCons = rotationConstants
        rotated = listArray ((0,0),(4,4)) $ zipWith rotateLane (elems bits) rotCons
        newX' (_,y) = y
        newY' (x,y) = (2*x+3*y) `mod` 5
    in
        listArray ((0,0),(4,4)) $ newPositions rotated newX' newY'

{- newPositions bits newX newY
   Assigns new positions in a array, based on the algorithm.
   PRE: size 1600 bits. newX,newY taken mod 5.
   RETURNS: A list with the new positions of the lanes.
-}
newPositions bits newX newY =
    let
        coordinates = indices bits
        newCoordinates = map (\(x,y)-> (newX (x,y),newY (x,y))) coordinates
        newPositions = zip newCoordinates coordinates
    in
        map (\((_,_),lane)-> lane) . sort . map (\((a,b),(c,d))->((a,b),bits!(c,d))) $ newPositions


{- chi bits
   The chi step of Keccak.
   PRE: bits has dimension (5,5,64)
   RETURNS: A array of dimension (5,5,64).
-}
chi bits =
    let
        newX' (x,_) = (x+1) `mod` 5
        newX'' (x,_) = (x+2) `mod` 5
        newY' (_,y) = y
        l1 =  complementLane $ newPositions bits newX' newY'
        l2 =  newPositions bits newX'' newY'
        l12 = zipWith (zipWith (.&.)) l1 l2
    in
        listArray ((0,0),(4,4)) $ zipWith (zipWith xor) (elems bits) l12

{- complementLane bitList
   Bitwise complement of a lane.
   PRE: length bitList == 64.
   RETURNS: The bitwise complement of bitList.
-}
complementLane bitList = map (map (\n-> if n == 1 then 0 else 1)) bitList

{- iota n bits
   Changes the (0,0) Lane to a specific value.
   PRE: n is between 0 and 23. bits dimensions is (5,5,64)
   RETURNS:
-}
iota n bits = listArray ((0,0),(4,4)) (new0 ++ (drop 1 (elems bits)))
    where new0 = [zipWith xor (bits!(0,0)) (roundC n)]


{-================================ Constants =================================-}
{-============================================================================-}

{- rotationConstants
-}
rotationConstants = [0, 36, 3, 41, 18,
                     1, 44, 10, 2, 45,
                     62, 6, 43, 15, 61,
                     28, 55, 25, 21, 56,
                      27, 20, 39, 8, 14]

 {- roundC
 -}
roundC 0  = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
roundC 1  = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0]
roundC 2  = [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             1,0,0,0,0,0,0,0,1,0,0,0,1,0,1,0]
roundC 3  = [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
roundC 4  = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             1,0,0,0,0,0,0,0,1,0,0,0,1,0,1,1]
roundC 5  = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
roundC 6  = [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1]
roundC 7  = [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1]
roundC 8  = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,1,0,0,0,1,0,1,0]
roundC 9  = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0]
roundC 10 = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1]
roundC 11 = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0]
roundC 12 = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             1,0,0,0,0,0,0,0,1,0,0,0,1,0,1,1]
roundC 13 = [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,1,0,0,0,1,0,1,1]
roundC 14 = [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             1,0,0,0,0,0,0,0,1,0,0,0,1,0,0,1]
roundC 15 = [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1]
roundC 16 = [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0]
roundC 17 = [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0]
roundC 18 = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             1,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0]
roundC 19 = [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0]
roundC 20 = [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1]
roundC 21 = [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0]
roundC 22 = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
roundC 23 = [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0]
