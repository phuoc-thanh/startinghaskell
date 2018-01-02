{-# OPTIONS_GHC -Wall #-}
module Haskell_IO (main) where

import qualified Data.ByteString as BS
import qualified Data.HashTable.IO as H
import Data.ByteString (ByteString)
import Data.Functor
import Data.Maybe
import Data.Word8
import Control.Monad


main :: IO ()
main = jabber


sillyExchange :: IO ()
sillyExchange = do
    putStrLn ("Hello, user!")
    putStrLn ("What's your name?")
    name <- getLine
    putStrLn $ "Please to meet you, " ++ name ++ "!"

jabber :: IO ()
jabber = do
  wocky <- readFile "jabberwocky.txt"
  let wockylines = drop 2 (lines wocky)  -- discard title
  count <- printFirstLines wockylines
  putStrLn $ "There are " ++ show count ++ " stanzas in Jabberwocky."

printFirstLines :: [String] -> IO Int
printFirstLines ls = do
  let first_lines = extractFirstLines ls
  putStr (unlines first_lines)
  return $ length first_lines

extractFirstLines :: [String] -> [String]
extractFirstLines []         = []
extractFirstLines [_]        = []
extractFirstLines ("" : first : rest)
  = first : extractFirstLines rest
extractFirstLines (_ : rest) = extractFirstLines rest

getWords :: IO [ByteString]
getWords = BS.split 32 <$> BS.getLine      
-- getWords = do
--   ln <- BS.getLine
--   return $ BS.split 32 ln -- 32 is the ASCII code for ' '


type HashTable k v = H.LinearHashTable k v

-- foo :: IO (HashTable Int Int)
foo = do
    ht <- H.new :: IO (HashTable Int Int)
    forM_ [1..1000000] $ \x -> H.insert ht x x
    n <- H.lookup ht 79909
    print n
    -- return ht