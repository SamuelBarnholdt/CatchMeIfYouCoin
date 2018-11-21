module WalletTypes where

import TransactionsandBlockchain
import TransactionandBlockchainTypes
import Cryptography

{- Name
   Data description - abstraction of a name
   Invariant -
-}
type Name = String

{- Names
   Data description - list of Name
   Invariant -
-}
type Names = [Name]

{- Password
   Data description - abstraction of a password
   Invariant -
-}
type Password = String

{- Account
   Data description - abstraction of an account
   Invariant -
-}
type Account = ((Name,Password),(PvtKey,PubKey))

{- Accounts
   Data description - list of Account
   Invariant - Should not contain several users with the same name and keys
-}
type Accounts = [Account]

{- PendingTransactions
   Data description - list of UnverifiedBlock, represents transaction waiting to
                       be mined onto the ledger
   Invariant -
-}
type PendingTransactions = [UnverifiedBlock]
