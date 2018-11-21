module TransactionandBlockchainTypes where

import InputOutputs
import Cryptography

{- Ledger
   Description - The blockchain.
   Invariant   - The net of all inputs must be equal to the net of all outputs.
                 The Index of all blocks has to be correct according to the spot in the
                 list and, the hash must be the hash of the previous block.
-}
type Ledger = [Block]

{- Block
   Description - Block containing all transactions that have been made. Index represents
                 the place in the ledger, hash is the hash of the previousblock, and
                 verifiedtransactions is the list of all transactions in that block.
   Invariant   - None
-}
data Block = Block Index Hash VerifiedTransactions deriving (Show, Read, Eq)

{- UnverifiedBlock
   Description  - Block with transactions, waiting to be verified in the ledger.
   Invariant    - None
-}
data UnverifiedBlock = UBlock Transactions deriving (Show,Read)

{- VerifiedTransaction
   Description  - Verified transaction. TransactionID represents the index in
                  the transactionlist in the block.

   Invariant    - For a transaction to be verified it's inputs must be found as
                  outputs in the ledger, and the net of all outputs can't be higher
                  than the inputs.
-}
data VerifiedTransaction = VerifiedTransaction TransactionID Inputs VerifiedOutputs deriving (Show, Read,Eq)

{- Transaction
   Description  - Unverified transaction
   Invariant    - None
-}
data Transaction = Transaction Inputs Outputs Sign deriving (Show,Read)

{- Transactions
   Description  - List of transactions.
   Invariant    - None
-}
type Transactions = [Transaction]

{- VeriedTransactions
   Description  - List of verified transactions.
   Invariant    - None
-}
type VerifiedTransactions = [VerifiedTransaction]

{- UTXO
   ("UTXO is geek-speak for “unspent transaction output."")
   Description  - List of all "unused" output, meaning they haven't been used
                  as input in the corresponding video.
   Invariant    - None
-}
type UTXO = VerifiedOutputs
