module Interface where

import System.IO
import Wallets
import Cryptography
import TransactionsandBlockchain
import TransactionandBlockchainTypes
import WalletTypes
import InputOutputs
import Control.Exception
import Data.Char
import System.Directory

--------------------------------------------------------------------------------
-- Mainwindow

{- main
   Pre - True
   Post - Instructs the user to press enter to log in, or write account
          to create an account. Then goes to main2.
   Examples -
-}
main :: IO ()
main = do
  putStrLn ""
  putStrLn "Welcome to CatchMeIfYouCoin!"
  putStrLn ""
  putStrLn "          ¢  ¢           "
  putStrLn "        ¢¢¢¢¢¢¢¢¢¢       "
  putStrLn "      ¢¢¢ ¢  ¢           "
  putStrLn "    ¢¢¢   ¢  ¢           "
  putStrLn "    ¢¢¢   ¢  ¢           "
  putStrLn "    ¢¢¢   ¢  ¢           "
  putStrLn "      ¢¢¢ ¢  ¢           "
  putStrLn "        ¢¢¢¢¢¢¢¢¢¢       "
  putStrLn "          ¢  ¢           "
  putStrLn ""
  putStrLn "If you wish to login then please press Enter."
  putStrLn ""
  putStrLn "If you wish to create an account, type account."
  putStrLn ""
  putStrLn "To exit the application, type exit."
  putStrLn ""
  main2

{- main2
   Pre - True
   Post - Takes the user to the login window or account creation depending on
          input.
   Examples -
-}
main2 :: IO ()
main2 = do
    input <- getLine
    if (map toLower input == "account") then createAccount
        else if input == "" then logIn
          else if map toLower input == "exit"
            then exit
            else do
              putStrLn ""
              putStrLn "We didn't get quite get that, please try again."
              putStrLn ""
              main2

--------------------------------------------------------------------------------
-- Account creation

{- createAccount
   Pre - True
   Post - First step of account creation, gets desired username from user,
          if the name exists it asks for another, if it does not it moves
          on to the next step.
   Examples -
-}
createAccount :: IO ()
createAccount = do
  putStrLn ""
  putStrLn "Type in your desired username."
  putStrLn ""
  username <- getLine
  handle <- openFile "accounts.txt" ReadMode
  users <- hGetContents handle
  if map toLower username == "exit"
    then main
    else if userExists (map toLower username) (read users :: Accounts)
      then do putStrLn ""
              putStrLn "That username is not available."
              putStrLn ""
              createAccount
                else createAccountName username
{- createAccountName name
   Pre - True
   Post - Makes sure name is the desired username ,
          if it was it moves on to passwordCreation, if not it goes back to
          createAccount
   Examples -
-}
createAccountName :: Name -> IO ()
createAccountName name = do
  putStrLn ""
  putStrLn "If this is your desired username write yes, if not write no."
  putStrLn ""
  input <- getLine
  if (map toLower input == "yes") then do passwordCreation (map toLower name)
      else if (map toLower input == "no") then createAccount
        else do
          putStrLn ""
          putStrLn "We didn't get quite get that, please type yes or no."
          putStrLn ""
          createAccountName name

{- passwordCreation name
   Pre - True
   Post - makes the user create a password. when done, it creates an account
          with name and the created password.
   Examples -
-}
passwordCreation :: Name -> IO ()
passwordCreation name = do
  putStrLn ""
  putStrLn "Type in your desired password."
  putStrLn ""
  pass1 <- getLine
  putStrLn ""
  putStrLn "Write your password again."
  putStrLn ""
  pass2 <- getLine
  if (pass1 == pass2) then do
      let prv = genPrivateKey pass2
          pub = Wallets.genPublicKey prv
      putStrLn ""
      putStrLn "Welcome! Please log in."
      putStrLn ""
      handle <- openFile "accounts.txt" ReadMode
      users <- hGetContents handle
      removeFile "accounts.txt"
      writeFile "accounts.txt" (show (addAccount ((name,pass2),(prv,pub)) (read users :: Accounts)) )
      logIn
      else
      passwordCreation name



{- logIn
   Pre - True
   Post - asks for the users username, if it exists in the account list it moves on to logIn2
          with input username and account list.
          if not it asks for it again. If the users writes account it moves
          to createAccount.
   Examples -
-}
logIn :: IO ()
logIn = do
  putStrLn ""
  putStrLn "What's your username?"
  putStrLn ""
  username <- getLine
  accountshandle <- openFile "accounts.txt" ReadMode
  accounts <- hGetContents accountshandle
  if userExists (map toLower username) (read accounts :: Accounts)
    then logIn2 (map toLower username) (read accounts :: Accounts)
      else
        do putStrLn ""
           putStrLn "User doesn't exist."
           putStrLn ""
           putStrLn "If you wish to create a new account, type account, else press enter."
           putStrLn ""
           input <- getLine
           if map toLower input == "account"
             then createAccount
              else logIn

{- logIn2 username accounts
   Pre - username must exist in accounts.txt
   Post - asks for a password and verifys it in accounts, if it's incorrect it
          repeats, if it's correct it looks up the users corresponding keys, and
          moves on to the interface.
   Examples -
-}
logIn2 :: Name -> Accounts -> IO ()
logIn2 username accounts = do
 putStrLn ""
 putStrLn "Type in your password."
 putStrLn ""
 password <- getLine
 if verifyAccount (username,password) accounts
   then do
     putStrLn ""
     help
     interface $ lookUpPersonalKeys username accounts
    else
      do putStrLn ""
         putStrLn "Incorrect password"
         putStrLn ""
         logIn2 username accounts

{- interface (pvtkey,publickey)
   Pre - True
   Post - gives the alternatives to make a transaction, to check balance,
          to logout and to exit
   Examples -
-}
interface :: (PvtKey,PubKey) -> IO ()
interface (pvtkey,publickey)= do
  input <- getLine
  if map toLower input == "balance"
    then balance (pvtkey,publickey)
      else if  map toLower input == "transaction"
        then transaction (pvtkey,publickey)
        else if map toLower input == "users"
          then users (pvtkey,publickey)
          else if map toLower input == "help"
            then do
              help
              interface (pvtkey,publickey)
              else do
                if map toLower input == "exit"
                  then exit
                    else if map toLower input == "logout"
                      then main
                        else do
                          putStrLn ""
                          putStrLn "We didn't quite get that, please try again."
                          putStrLn ""
                          putStrLn "To get possible commands, type help."
                          putStrLn ""
                          interface (pvtkey,publickey)



--------------------------------------------------------------------------------
-- help option
{- help
   Pre - True
   Post - Prints out all user options
   Examples -
-}
help :: IO ()
help = do
  putStrLn ""
  putStrLn "Welcome to your wallet!"
  putStrLn ""
  putStrLn "To check your balance , type balance."
  putStrLn ""
  putStrLn "To make a transaction, type transaction."
  putStrLn ""
  putStrLn "To find users to send money to, type users."
  putStrLn ""
  putStrLn "To logout from the application, type logout."
  putStrLn ""
  putStrLn "To exit the application, type exit."
  putStrLn ""
--------------------------------------------------------------------------------
-- balance option
{- balance (pvtkey,publickey)
   Pre - publickey must be tied to an account in accounts.txt
   Post - prints out user balance, then returns to the interface
   Examples -
-}
balance :: (PvtKey,PubKey) -> IO ()
balance (pvtkey,publickey) = do
  handle <- openFile "ledger.txt" ReadMode
  contents <- hGetContents handle
  putStrLn ""
  putStrLn $ show $ totalInput (createPersonalUTXO (read contents :: Ledger) publickey)
  putStrLn ""
  interface (pvtkey,publickey)

--------------------------------------------------------------------------------
-- users option

{- users (pvtkey,publickey)
   Pre - True
   Post - prints out all existing users, then returns to the interface
   Examples -
-}
users :: (PvtKey,PubKey) -> IO ()
users (pvtkey, publickey) = do
  handle <- openFile "accounts.txt" ReadMode
  contents <- hGetContents handle
  putStrLn ""
  mapM putStrLn $ getUsers (read contents :: Accounts)
  putStrLn ""
  interface (pvtkey,publickey)



--------------------------------------------------------------------------------
-- Transaction option

{- transaction (pvtkey,pubkey)
   Pre - True
   Post - asks for someone to send a transaction to, moves on to transactionRecipients
          with the answer if the answer exists in accounts.txt, or prints out users if
          the answer is users
   Examples -
-}
transaction :: (PvtKey,PubKey) -> IO ()
transaction (pvtkey,pubkey) = do
  putStrLn ""
  putStrLn "If you want to see available recipients, type users."
  putStrLn ""
  putStrLn "Who would you like to send money to?"
  putStrLn ""
  handle <- openFile "accounts.txt" ReadMode
  users <- hGetContents handle
  answer <- getLine
  if map toLower answer == "users"
    then do
      putStrLn ""
      usersForTransactions (pvtkey,pubkey)
      else if userExists (map toLower answer) (read users :: Accounts)
        then transactionRecipients (pvtkey,pubkey) [answer]
          else do
            putStrLn ""
            putStrLn "That user does not exist."
            transaction (pvtkey,pubkey)


{- transactionRecipients (pvtkey,pubkey) recipients
   Pre - True
   Post - asks if the user would like to send money to anyone else, if no
          it moves on to makeTransaction. if yes it moves to transactionRecipients'
   Examples -
-}
transactionRecipients :: (PvtKey, PubKey) -> Names -> IO ()
transactionRecipients (pvtkey,pubkey) recipients = do
  ledgerhandle <- openFile "ledger.txt" ReadMode
  ledger <- hGetContents ledgerhandle
  let balance = totalInput $ createPersonalUTXO (read ledger :: Ledger) pubkey
  putStrLn ""
  putStrLn "Would you like to send money to anyone else?"
  putStrLn ""
  answer <- getLine
  if map toLower answer == "no"
    then makeTransaction (pvtkey, pubkey) recipients [] balance
      else if map toLower answer == "yes"
        then transactionRecipients' (pvtkey,pubkey) recipients
            else do
              putStrLn ""
              putStrLn "We didn't quite get that, please try again."
              putStrLn ""
              transactionRecipients (pvtkey,pubkey) recipients

{- transactionRecipients' (pvtkey,pubkey)
   Pre - True
   Post - second step of transactionrecipients, asks for another user to send
          money to
   Examples -
-}
transactionRecipients' :: (PvtKey,PubKey) -> Names -> IO ()
transactionRecipients' (pvtkey,pubkey) recipients = do
  handle <- openFile "accounts.txt" ReadMode
  users <- hGetContents handle
  putStrLn ""
  putStrLn "Who would you like to send money to?"
  putStrLn ""
  answer <- getLine
  if userExists (map toLower answer) (read users :: Accounts)
    then do
      putStrLn ""
      transactionRecipients (pvtkey,pubkey) (answer:recipients)
      else do
        putStrLn ""
        putStrLn "That user does not exist."
        putStrLn ""
        transactionRecipients' (pvtkey,pubkey) recipients


{- makeTransaction (pvtkey,pubkey) recipients@(x:xs) outputs balance
   Pre - balance can't be negative , pubkey must be tied to an account
         in accounts.txt,
   Post -  for every recipient in recipients, asks for an amount
            to send and creates an output which it appends to outputs. When
            recipients is empty it selects inputs from the the personal utxo corresponding
            to input pubkey, and makes a transaction from all outputs
   Examples -
-}
makeTransaction :: (PvtKey,PubKey) -> Names -> Outputs -> Amount -> IO ()
makeTransaction (pvtkey, pubkey) [] outputs balance = do
  ptransactionhandle <- openFile "pendingtransactions.txt" ReadMode
  ptransactions <- hGetContents ptransactionhandle
  ledgerhandle <- openFile "ledger.txt" ReadMode
  ledger <- hGetContents ledgerhandle
  let personalutxo = createPersonalUTXO (read ledger :: Ledger) pubkey
  let (inputs,returnoutput) = selectTransactions personalutxo [] (totalOutput outputs) pubkey
  let transaction = createTransaction inputs (returnoutput ++ outputs) pvtkey
  let newpendingtransactions = addPendingTransaction transaction (read ptransactions :: PendingTransactions)
  removeFile "pendingtransactions.txt"
  writeFile "pendingtransactions.txt" $ show newpendingtransactions
  putStrLn ""
  putStrLn "Thank you for using CatchMeIfYouCoin!"
  putStrLn ""
  putStrLn "Your transaction will be published on the ledger shortly."
  putStrLn ""
  interface (pvtkey,pubkey)
makeTransaction (pvtkey, pubkey) (x:xs) outputs balance = do
  accountshandle <- openFile "accounts.txt" ReadMode
  accounts <- hGetContents accountshandle
  putStrLn ""
  putStrLn $ "How much money would you like to send to" ++ " " ++ x ++ "?"
  putStrLn ""
  let recipientpubkey = snd (lookUpPersonalKeys x (read accounts :: Accounts))
  input <- getLine
  if input == "exit"
    then interface (pvtkey,pubkey)
      else if containsCharacters input || head input == '0'
        then do
          putStrLn ""
          putStrLn "We didn't get quite get that, please try again."
          putStrLn ""
          makeTransaction (pvtkey, pubkey) (x:xs) outputs balance
            else if (read input :: Integer) > balance
              then do
                putStrLn ""
                putStrLn "You don't have enough coins to send that amount."
                putStrLn ""
                putStrLn $ "Your remaining balance is" ++ " " ++ (show balance) ++ "."
                putStrLn ""
                makeTransaction (pvtkey, pubkey) (x:xs) outputs balance
                  else do
                    let amount = (read input :: Integer)
                    makeTransaction (pvtkey, pubkey) xs ((recipientpubkey,amount):outputs) (balance - amount)


{- usersForTransactions (pvtkey, publickey)
   Pre - True
   Post - prints out all existing users, then returns to transaction
   Examples -
-}
usersForTransactions :: (PvtKey,PubKey) -> IO ()
usersForTransactions (pvtkey, publickey) = do
  handle <- openFile "accounts.txt" ReadMode
  contents <- hGetContents handle
  putStrLn ""
  mapM putStrLn $ getUsers (read contents :: Accounts)
  putStrLn ""
  transaction (pvtkey,publickey)

--------------------------------------------------------------------------------
--exitwindow

{- exit
   Pre - True
   Post - exits from the program
   Examples -
-}
exit :: IO ()
exit = do
  putStrLn ""
  putStrLn "Thank you for supporting CatchMeIfYouCoin!"
  putStrLn ""
  putStrLn "We hope to see you again!"
  putStrLn ""
  return ()
--------------------------------------------------------------------------------
-- Miner

{- miner
   Pre - True
   Post - reads from pendingtransactions.txt, and tries to append the head of the list
          to the ledger in ledger.txt, then updates pendingtransactions.txt

          if pendingtransactions.txt contains an empty list, it tells the user
          that there are no pending transactions and exits
   Examples -
-}
miner :: IO ()
miner = do
  ledgerhandle <- openFile "ledger.txt" ReadMode
  ledger1 <- hGetContents ledgerhandle
  phandle <- openFile "pendingtransactions.txt" ReadMode
  pendingtransaction <- hGetContents phandle
  let pendingtransactions = read pendingtransaction :: PendingTransactions
  let ledger = read ledger1 :: Ledger
  if null pendingtransactions
    then do
      putStrLn ""
      putStrLn "There are no pending transactions at the moment."
      putStrLn ""
      putStrLn "Thank you for contributing to CatchMeIfYouCoin."
      putStrLn ""
      else do
        removeFile "ledger.txt"
        writeFile "ledger.txt" $ show (addBlock (head pendingtransactions) ledger)
        removeFile "pendingtransactions.txt"
        writeFile "pendingtransactions.txt" $ show (tail pendingtransactions)
        putStrLn ""
        putStrLn "Thank you for contributing to CatchMeIfYouCoin."
        putStrLn ""
