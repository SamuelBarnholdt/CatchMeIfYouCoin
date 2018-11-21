-- DO NOT MODIFY THE FOLLOWING LINES

module InputOutputs where
import Cryptography
import Data.Hashable

-- END OF DO NOT MODIFY ZONE

--------------------------------------------------------------------------------
-- Types

{- Index
   Data description - Index, refering to block position in ledger.
   Invariant - Can't be 0, and has to be correct according to the block position.
-}
type Index = Integer

{- Hash
   Data description - hash of any type
   Invariant - None
-}
type Hash = Integer

{- Output
   Data description - proposed transaction where the PubKey represents recipient
                      and Amount represents the amount
   Invariant -
-}
type Output = (PubKey, Amount)

{- TransactionID
   Data description - Index of transactionlist for finding
   Invariant -
-}
type TransactionID = Integer

{- Amount
   Data description - Representation of amount of coins.
   Invariant - has to be greater or equal to zero
-}
type Amount = Integer

{- VerifiedOutput
   Data description - representation of an unspend output that hasn't been made,
                      pubkey represents recipient, amount the amount,
                      the hash is the hash of the previous block in the ledger
                      transactionID represents the index in the transactionlist
                      where the output is found
   Invariant - needs to satisfy all type invariants
-}
type VerifiedOutput = (PubKey,Amount,Hash,Index,TransactionID)

{- Input
   Data description - representation of a spent input,
                      pubkey represents recipient, amount the amount,
                      the hash is the hash of the previous block in the ledger
                      transactionID represents the index in the transactionlist
                      where the output is found
   Invariant - needs to satisfy all type invariants
-}
type Input = VerifiedOutput


-- lists of types
type VerifiedOutputs = [VerifiedOutput]
type Inputs = VerifiedOutputs
type Outputs = [Output]


--------------------------------------------------------------------------------
-- Functions



{- sameKeys inputs
   Pre - True
   Post - returns true if all inputs have the same key
   Examples -
-}
sameKeys :: Inputs -> Bool
sameKeys inputs = notElem False (map (==key) keylist)
  where key = head keylist
        keylist = map getInputKey inputs

{- getInputKey input
   Pre - True
   Post - returns the pubkey in input
   Examples -
-}
getInputKey :: Input -> PubKey
getInputKey (pkey,_,_,_,_) = pkey


{- getInputKey input
   Pre - True
   Post - returns the hash in input
   Examples -
-}
getInputHash :: Input -> Hash
getInputHash (_,_,hash,_,_) = hash

{- totalOutput outputs
   Pre - True
   Post - adds all amounts from the outputs together
   Examples -
-}
totalOutput :: Outputs -> Integer
totalOutput [] = 0
totalOutput ((_,int):xs) = int + totalOutput xs

{- totalInput inputs
   Pre - True
   Post - adds all amounts from the inputs together
   Returns -
-}
totalInput :: Inputs -> Integer
totalInput [] = 0
totalInput ((_,int,_,_,_):xs)= int + totalInput xs
