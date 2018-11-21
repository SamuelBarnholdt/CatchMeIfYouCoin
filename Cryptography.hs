module Cryptography where

import Test.HUnit

type Sign = (Integer,Integer)
type PubKey = (Integer,Integer)
type PvtKey = Integer

{- base58 decimal
   Converts a number in base 10 to a string of base 58.
   PRE: True
   RETURNS: The number in base 58 as a string.
   EXAMPLES: base58 12345678 =
   Needs to use integer.
-}
base58 :: Int ->  String
base58 0 = ""
base58 n = base58 ((abs n) `div` 58) ++ (b58 !! ((abs n) `mod` 58)):""
    where b58 = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

{- addElliptic point1 point2
   Point addition on secp256k1.
   PRE: Points must be in the field.
   RETURNS: A tuple with x and y coordinates for the resulting sum.
   EXAMPLES:
-}
addElliptic p1@(x1,y1) p2@(x2,y2) =
    let k | x1 == x2 && y1 == -y2 || -y1 == y2 = error "Point at infinity"
          | p1 /= p2 = ((y1-y2) * (fromJust . modInv (if (x1-x2) > 0 then x1-x2 else (x1-x2) + p ) $ p)) `mod` p
          | p1 == p2 = ((3*x1^2) *(fromJust . modInv (2*y1) $ p)) `mod` p
        x = (k^2 - x1 - x2) `mod` p
        y = (k * (x1 - x) - y1) `mod` p
        p = 2^256 - 2^32 - 2^9 - 2^8 - 2^7 - 2^6 - 2^4 - 1
    in (x,y)


gcdExt a 0 = (1, 0, a)
gcdExt a b = let (q, r) = a `quotRem` b
                 (s, t, g) = gcdExt b r
             in (t, s - q * t, g)

-- Given a and m, return Just x such that ax = 1 mod m.  If there is no such x
-- return Nothing.
-- Stolen buggy piece of shit. ===============
--- ==========================================
modInv a m = let (i, _, g) = gcdExt a m
             in if g == 1 || g == -1 then Just (mkPos i) else Nothing
  where mkPos x = if x < 0 then x + m else x


fromJust (Just x) = x

{- doubleAndAdd number generator
   Generates a public key from the private key.
   PRE: Must be a string of binary number.
   Returns: A tuple where the first value is the private key
            and the second is the public key.
   EXAMPLES:
   VARIANT: length binaryRep
-}
doubleAndAdd [] result g = result
doubleAndAdd (x:binaryRep) result g
    | x == 0 = doubleAndAdd binaryRep (addElliptic result result) g
    | x == 1 = doubleAndAdd binaryRep (addElliptic g $ addElliptic result result) g

toBinary 0 = []
toBinary n = ((abs n) `mod` 2) : toBinary ((abs n) `div` 2)

binary = reverse . toBinary

{- scalarMult privateKey generator
   Generates a public key with the private key and the generator poin.
   PRE:
   Returns: A tuple with x and y coordinates of the private key.
   EXAMPLES:
-}
scalarMult p g
    | p == 1 = g
    | [1,0] == l = doubleAndAdd (drop 2 $ binary p) (addElliptic g g) g
    | [1,1] == l = doubleAndAdd (drop 2 $ binary p) (addElliptic g $ addElliptic g g) g
    where l = (take 2 $ binary p)

-- Generates a signature. Transaction is hashed.
-- Does r make sense?

{- signature transaction privateKey randomK
   Signs a transaction.
   PRE: Transaction must be hashed. privateKey and
        randomK must be of 256 bit length.
   RETURNS: A tuple with values needed for verification.
   EXAMPLES:
-}
signature transaction privateKey randomK =
    let ord    = 115792089237316195423570985008687907852837564279074904382605163141518161494337
        g      = (55066263022277343669578718895168534326250603453777594175500187360389116729240, 32670510020758816978083085130507043184471273380659243275938904335757337482424)
        (xp,_) = scalarMult randomK g
        r      = xp `mod` ord
        kInv   = fromJust $ modInv randomK ord
    in (r, kInv * (transaction + r * privateKey))
-- Add cases in which new randomK must be generated.

{- verifySignature publicKey transaction signature
    Checks if a signature is valid.
    PRE: publicKey 256 bit long and transaction must be hashed.
    RETURNS: True if signature is valid, else False.
    EXAMPLES:
-}
verifySignature publicKey transaction (r,s) =
    let ord    = 115792089237316195423570985008687907852837564279074904382605163141518161494337
        g      = (55066263022277343669578718895168534326250603453777594175500187360389116729240, 32670510020758816978083085130507043184471273380659243275938904335757337482424)
        sInv   = fromJust $ modInv s ord
        u1     = (sInv * transaction) `mod` ord
        u2     = (sInv * r) `mod` ord
        (xp,_) = addElliptic (scalarMult u1 g) (scalarMult u2 publicKey)
    in r == xp `mod` ord

{- genPublicKey privateKey
   Generates a public key.
   PRE: privateKey is 256 bits.
   RETURNS: A tuple with x and y coordinates of the public key.
   EXAMPLES:
-}
genPublicKey privateKey = privateKey `scalarMult` g
    where g = (55066263022277343669578718895168534326250603453777594175500187360389116729240, 32670510020758816978083085130507043184471273380659243275938904335757337482424)
