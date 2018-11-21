module Wallets where

import Cryptography
import TransactionandBlockchainTypes
import TransactionsandBlockchain
import InputOutputs
import qualified SHA3_256 as SHA3
import WalletTypes
--------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Accounts functions

{- verifyAccount (name,password) accounts
   Pre - True
   Post - returns true if the input exists in accounts
   Examples -
-}
verifyAccount :: (Name,Password) -> Accounts -> Bool
verifyAccount s [] = False
verifyAccount s a
 | s == fst (head a) = True
 | otherwise   = verifyAccount s (drop 1 a)

{- userExists name accounts
   Pre - True
   Post - checks if the name exists in accounts, returns True if it does.
   Examples -
-}
userExists :: Name -> Accounts -> Bool
userExists _ [] = False
userExists name (((name1,_),(_,_)):xs)
  | name == name1 = True
  | otherwise = userExists name xs


{- addAccount account accounts
   Pre - True
   Post - adds account to accounts
   Examples -
-}
addAccount :: Account -> Accounts -> Accounts
addAccount account accounts = account:accounts

{- lookUpPersonalKeys name accounts
   Pre - name should be found in accounts
   Post - returns users personal keys
   Examples -
-}
lookUpPersonalKeys :: Name -> Accounts -> (PvtKey,PubKey)
lookUpPersonalKeys name accounts
  | name == fst (fst (head accounts)) = snd (head accounts)
  | otherwise = lookUpPersonalKeys name (tail accounts)

{- sameKeys inputs
   Pre - True
   Post - returns true if all inputs have the same key
   Examples -
-}
getUsers :: Accounts -> [Name]
getUsers accounts = map (fst . fst) accounts


--------------------------------------------------------------------------------

-- Transaction functions


{- createTransaction userpvtkey outputs input
   Pre - True
   Post - Creates a transaction, where the input specifies amount and output
          specifies how much and to who. The userpvtkey is for signing the
          transaction.
   Examples -
-}
createTransaction :: Inputs -> Outputs -> PvtKey -> Transaction
createTransaction input outputs userpvtkey = Transaction input outputs signed
    where hsh = hashTransaction input outputs
          signed = sign hsh userpvtkey 234245363652

{- selectTransactions inputs accinput amount pubkey
   Pre - amount can't be greater than the total inputamount in
         inputs
   Post - selects inputs to use for sending the input amount. If there is
          "change", it sends an output back to the owner of the pubkey containing
          the difference.
   Examples -
-}
selectTransactions :: Inputs -> Inputs -> Amount -> PubKey -> (Inputs,Outputs)
selectTransactions (input@(pkey,inputamount,hash,index,transid):xs) accinput amount key
  | amount == inputamount = (input:accinput,[])
  | amount < inputamount = (input:accinput,[(key,inputamount-amount)])
  | otherwise = selectTransactions xs (input:accinput) (amount-inputamount) key


{- addPendingTransaction
   Pre - True
   Post - adds a pending transaction to the list of pendingtransactions
   Examples -
-}
addPendingTransaction :: Transaction -> PendingTransactions -> PendingTransactions
addPendingTransaction transaction [] = [createUBlock transaction]
addPendingTransaction transaction (x@(UBlock transactions):xs)
  | length transactions > 4 = x:(addPendingTransaction transaction xs)
  | otherwise = (UBlock (transaction:transactions)):xs

--------------------------------------------------------------------------------
-- Stupid functions

{- containsCharacters string
   Pre - True
   Post - Returns true if the input string contains anything that's not numbers
   Examples -
-}
containsCharacters :: String -> Bool
containsCharacters [] = False
containsCharacters (x:xs)
  | x == '1' || x == '2' || x == '3' || x == '4' || x == '5' || x == '6' || x == '7' || x == '8' || x == '9' || x == '0' = containsCharacters xs
  | otherwise = True

{- ellipticCurve xCoordinate
 Calculates the y-value of the elliptic curve.
 PRE: x must be on the field.
 RETURNS: A tuple with the x and y coordinate.
 EXAMPLES:
-}
ellipticCurve x =
  let ord = 115792089237316195423570985008687907852837564279074904382605163141518161494337
      y   = sqrt $ (x^3 + 7) `mod` ord
  in (x,y)

{- genPrivateKey IO
 Generates a pseudorandom number.
 PRE: True
 RETURNS: A 256 bit Integer.
 EXAMPLES:
-}
genPrivateKey = SHA3.sha3_256S

{- genPublicKey privateKey
 Generates a public key.
 PRE: ?
 RETURNS: A tuple with the x and y coordinates of the public key.
-}
genPublicKey = Cryptography.genPublicKey

sign = signature
