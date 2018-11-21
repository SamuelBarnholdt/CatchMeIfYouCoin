-- DO NOT MODIFY THE FOLLOWING LINES

module TransactionsandBlockchain where


import InputOutputs
import TransactionandBlockchainTypes
import Control.Monad
import System.IO
import System.Directory
import Test.HUnit
import Cryptography
import Data.Hashable
import Data.Text.Internal
import qualified SHA3_256 as SHA3
-- END OF DO NOT MODIFY ZONE

--------------------------------------------------------------------------------

-- All comments are found above the functions/datatypes

--------------------------------------------------------------------------------
-- Types and datatypes



--------------------------------------------------------------------------------

-- Ledger verification

{- verifyState ledger
   Pre - ledger needs to have length 2
   Post - Returns false should the ledger not meet the data invariant.
   Examples -
-}
verifyState :: Ledger -> Bool
verifyState [x] = True
verifyState (x:y:xs)
  | blockhash == hashBlock x && notElem False (map (==blockhash) transactionhashes) = verifyState (y:xs)
  | otherwise = False
    where blockhash = blockHash y
          transactions = blockTransaction y
          outputs = concatMap getVerifiedOutputs transactions
          transactionhashes = map getInputHash outputs

--------------------------------------------------------------------------------
-- Preliminary verification of transactions. This vericiation is to be done by nodes, before any UnverifiedBlock is created.

{- verifyTransaction ledger transaction
   Pre - True
   Post - True if the transaction is correct, or false if it's not.
          Correct meaning that all inputs can be found as outputs in the ledger,
          that the total of all output is not greater than the total of all input,
          and that all inputs contain the same key.
   Examples -
-}
verifyTransaction :: Ledger -> Transaction -> Bool
verifyTransaction ledger transaction@(Transaction inputs outputs sign) =
   totalInput inputs >= totalOutput outputs && sameKeys inputs && checkInputValidity utxo inputs
            && verifySignature pubkey hsh sign
    where utxo = createUTXO ledger []
          pubkey = pentaFst (head inputs)
          hsh = hashTransaction inputs outputs

pentaFst (a,b,c,d,e) = a

{- verifyInput ledger inputs
   Pre - True
   Post - Returns true if all inputs are correct according to verifyInput'
   Returns -

verifyInput :: Ledger -> Inputs -> Bool
verifyInput ledger inputs = notElem False $ map (verifyInput' ledger) inputs
  where
       {- verifyInput' ledger input
          Pre - True
          Post - Returns true if the input is correct according to function.
          Returns -
       -}
       verifyInput' :: Ledger -> Input -> Bool
       verifyInput' ledger input@(pubkey,amount,hash,index,transactionid)
         | length ledger < index - 1 = False
         | length transactions < transactionid - 1 = False
         | otherwise = elem True (map (==input) inputs)
           where block = ledger !! index
                 transactions = blockTransaction block
                 transaction = transactions !! transactionid
                 inputs = getVerifiedOutputs transaction
-}
--------------------------------------------------------------------------------
-- Final verification and adding of block to ledger

{- addBlock ublock ledger
   Pre - True
   Post - adds the ublock to the ledger if it can be verified
   Examples -
-}
addBlock :: UnverifiedBlock -> Ledger -> Ledger
addBlock ublock ledger
  | verifyBlock ublock ledger = addBlock' ublock ledger
  | otherwise = ledger

{- addBlock' ublock ledger
   Pre - True
   Post - transforms the ublock to a regular block and adds index hash and
          transactionids to all transactions, it then adds the block to the ledger
   Examples -
-}
addBlock' :: UnverifiedBlock -> Ledger -> Ledger
addBlock' ublock ledger = ledger ++ [newblock]
  where newblock = uBlockToBlock ublock index hsh
        index = blockIndex (last ledger) + 1
        hsh = hashBlock (last ledger)

{- verifyBlock ublock ledger
   Pre - True
   Post - Verifies the ublock in the ledger, by checking if all inputs are unspent.
   Examples -
-}
verifyBlock :: UnverifiedBlock -> Ledger -> Bool
verifyBlock (UBlock []) _ = False
verifyBlock ublock ledger = checkInputValidity utxo inputs && notElem False verifyinputs
  where transactions = uBlockTransactions ublock
        inputs = concatMap getInputs transactions
        utxo = createUTXO ledger []
        verifyinputs = map (verifyTransaction ledger) transactions



{- verifyTransactions hash index transactionid transactions
   Pre  - True
   Post - transforms all transactions to verifiedtransactions.
   Returns -
-}
verifyTransactions :: Hash -> Index -> TransactionID -> Transactions -> VerifiedTransactions
verifyTransactions hash index transactionid [] = []
verifyTransactions hash index transactionid (transaction:xs) =
  (VerifiedTransaction transactionid inputs verifiedoutputs):(verifyTransactions hash index (transactionid+1) xs)
        where inputs = getInputs transaction
              verifiedoutputs = verifyOutputs outputs hash index transactionid
              outputs = getOutput transaction

{- verifyOutputs outputs hash index transactionid
   Pre - True
   Post - Adds a transactionid hash and index to all outputs, creating
          verified outputs.
   Examples -
-}
verifyOutputs :: Outputs -> Hash -> Index -> TransactionID -> VerifiedOutputs
verifyOutputs [] hash index transid = []
verifyOutputs (x:xs) hash index transid =
  verifiedoutput:(verifyOutputs xs hash index transid )
      where verifiedoutput = verifyOutput x hash index transid
            {- verifyOutput output hash index transactionid
               Pre - True
               Post - adds the hash , index and transactionid to a output, making
                      it a verifed output.
               Examples -
            -}
            verifyOutput :: Output -> Hash -> Index -> TransactionID -> VerifiedOutput
            verifyOutput (pkey,amount) hash index transactionid = (pkey,amount, hash,index,transactionid)

{- checkInputValidity utxo inputs
   Pre - True
   Post - makes sure all inputs exist and that they are unspent. If they
          are the function returns True, else false.
   Examples -
-}
checkInputValidity :: UTXO -> Inputs -> Bool
checkInputValidity utxo [] = True
checkInputValidity utxo (x:xs)
  | elem x utxo = checkInputValidity (filter (/= x) utxo) xs
  | otherwise = False
--------------------------------------------------------------------------------
-- UTXO functions

{- createPersonalUTXO ledger pubkey
   Pre - True
   Post - creates a utxo that contains all unspent transactions corresponding
          to the pubkey.
   Examples -
-}
createPersonalUTXO:: Ledger -> PubKey -> UTXO
createPersonalUTXO ledger pubkey = filter ((pubkey ==) .(\(key,_,_,_,_) -> key)) utxo
  where utxo = createUTXO ledger []

{- createUTXO ledger utxo
   Pre - True
   Post - creates a utxo containing all unspent transactions in the ledger.
   Examples -
-}
createUTXO :: Ledger -> UTXO -> UTXO
createUTXO [] utxo = utxo
createUTXO (block:tails) utxo = createUTXO tails (createUTXO' utxo (blockTransaction block))
  where
       {- createUTXO' verifiedtransaction utxo
          Pre - True
          Post - filters all inputs that are found in utxo and adds all
                 verifiedoutputs to utxo.
          Examples -
       -}
       createUTXO' :: UTXO -> VerifiedTransactions -> UTXO
       createUTXO' utxo [] = utxo
       createUTXO' utxo (x:xs) = createUTXO' (outputUTXO' x (inputUTXO' x utxo)) xs

{- inputUTXO' verifiedtransaction utxo
   Pre - True
   Post - filters all inputs in verifiedtransaction from the utxo
   Examples -
-}
inputUTXO' :: VerifiedTransaction -> UTXO -> UTXO
inputUTXO' (VerifiedTransaction tid [] out) utxo = utxo
inputUTXO' (VerifiedTransaction tid (x:xs) out) utxo =
  inputUTXO' (VerifiedTransaction tid xs out) (filter (/= x) utxo)

{- outputUTXO' verifiedtransaction utxo
   Pre - True
   Post - adds all outputs in verifiedtransaction to utxo
   Examples -
-}
outputUTXO' :: VerifiedTransaction -> UTXO -> UTXO
outputUTXO' (VerifiedTransaction tid inp []) utxo = utxo
outputUTXO' (VerifiedTransaction tid inp (x:xs)) utxo =
  outputUTXO' (VerifiedTransaction tid inp xs) (x:utxo)



--------------------------------------------------------------------------------
-- Transactionoperations

{- getVerifiedOutputs verifiedtransaction
   Pre - True
   Post - returns the verifiedoutputs from verifiedtransaction
   Examples -
-}
getVerifiedOutputs :: VerifiedTransaction -> VerifiedOutputs
getVerifiedOutputs (VerifiedTransaction _ _ outputs) = outputs

{- getVerifiedInputs verifiedtransaction
   Pre - True
   Post - returns the inputs from verifiedtransaction
   Examples -
-}
getVerifiedInputs :: VerifiedTransaction -> Inputs
getVerifiedInputs (VerifiedTransaction _ inputs _) = inputs

{- getOutput transaction
   Pre - True
   Post - returns the outputs from the transaction
   Returns -
-}
getOutput :: Transaction -> Outputs
getOutput (Transaction _ outputs _) = outputs

{- getInputs transaction
   Pre - True
   Post - returns the inputs from the transaction
   Returns -
-}
getInputs :: Transaction -> Inputs
getInputs (Transaction inputs _ _) = inputs

--------------------------------------------------------------------------------

-- Block Operations

{- blockIndex block
   Pre - True
   Post - returns the index from the block
   Examples -
-}
blockIndex :: Block -> Index
blockIndex (Block index _ _) = index

{- blockHash block
   Pre - True
   Post - returns the hash from the block
   Examples -
-}
blockHash :: Block -> Hash
blockHash (Block _ hash _) = hash

{- blockTransaction block
   Pre - True
   Post - returns the transactions from the block
   Examples -
-}
blockTransaction :: Block -> VerifiedTransactions
blockTransaction (Block _ _ transactions) = transactions

--------------------------------------------------------------------------------
--  UnverifiedBlockoperations

{- createUBlock transaction
   Pre - True
   Post - creates a UnverifiedBlock from a transaction
   Examples -
-}
createUBlock :: Transaction -> UnverifiedBlock
createUBlock transaction = UBlock [transaction]

{- addTransactionToUBlock transaction ublock
   Pre - True
   Post - adds a transaction to ublock.
   Examples -
-}
addTransactionToUBlock :: Transaction -> UnverifiedBlock -> UnverifiedBlock
addTransactionToUBlock transaction (UBlock transactions) = UBlock (transaction:transactions)

{- uBlockTransactions ublock
   Pre - True
   Post - returns the transactions from the ublock
   Examples -
-}
uBlockTransactions :: UnverifiedBlock -> Transactions
uBlockTransactions (UBlock transactions) = transactions

{- uBlockToBlock ublock index hash
   Pre - True
   Post - turns input UBlock into a block by verifying its transactions
   Examples -
-}
uBlockToBlock :: UnverifiedBlock -> Index -> Hash -> Block
uBlockToBlock ublock index hash = Block index hash verifiedtransactions
  where verifiedtransactions = verifyTransactions hash index 0 transactions
        transactions = uBlockTransactions ublock

--------------------------------------------------------------------------------
-- Hash functions



{- hashBlock block
   Pre - True
   Post - hashes input block
   Examples -
-}
hashBlock :: Block -> Integer
hashBlock (Block index hsh verifiedtransaction) =
    SHA3.sha3_256 summation
        where summation = index + hsh + sum (map (hashVerifiedTransaction) verifiedtransaction)

{- hashVerifiedTransaction verifiedtransaction
  Pre - True
  Post - hashes verifiedtransaction
  Examples -
  -}
hashVerifiedTransaction :: VerifiedTransaction -> Integer
hashVerifiedTransaction (VerifiedTransaction transactionid inputs verifiedoutputs) =
    SHA3.sha3_256 summation
        where summation = transactionid + (addInputs inputs) +
                            (addInputs verifiedoutputs)

hashTransaction input outputs = SHA3.sha3_256 $ (addInputs input) + (addOutputs outputs)

addInputs [] = 0
addInputs (((a,f),b,c,d,e):xs) = a + b + c + d + e + addInputs xs

addOutputs [] = 0
addOutputs (((a,b),c):xs) = a + b + c + addOutputs xs
--------------------------------------------------------------------------------
