{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.Bits
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy.Char8 as B

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret fp1 fp2 = do
    modified <- BS.readFile fp1
    original <- BS.readFile fp2
    return $ BS.filter (/= 0) (BS.pack $ BS.zipWith xor modified original)

-- Exercise 2 -----------------------------------------

--to Test
--decryptWithKey (B.pack "Haskell Is Great!") "clues/victims.json"
decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey k f = do
    enc <- BS.readFile (f ++ ".enc")
    BS.writeFile f (BS.pack $ BS.zipWith xor repK enc)
        where repK = BS.pack . cycle $ BS.unpack k

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile file = do
    contents <- BS.readFile file
    return $ decode contents

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs v t = do
    victims <- parseFile v :: IO (Maybe [TId])
    transactions <- parseFile t :: IO (Maybe [Transaction])
    return $ filterT victims transactions

filterT :: Foldable t => Maybe (t TId) -> Maybe [Transaction] -> Maybe [Transaction]
filterT (Just vs) (Just ts) = Just $ filter (\t -> any (\v -> v == tid t) vs) ts
filterT _ Nothing = Nothing
filterT Nothing _ = Nothing


-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow transactions = foldr updateT Map.empty transactions

updateT :: Transaction -> Map String Integer -> Map String Integer
updateT t m = (Map.alter outAmount fromP . Map.alter inAmount toP) m
        where inAmount _  = Just (amount t)
              outAmount _ = Just (- (amount t))
              fromP       = from t
              toP         = to t

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = undefined

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs = undefined

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON = undefined

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

-- https://github.com/shinew/cis194/blob/master/05-IO/homework.hs
-- https://en.wikipedia.org/wiki/XOR_cipher
-- https://hackage.haskell.org/package/bytestring-0.10.8.1/docs/Data-ByteString-Lazy.html
-- https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Bits.html