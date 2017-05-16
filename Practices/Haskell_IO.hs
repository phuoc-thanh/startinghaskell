{-# OPTIONS_GHC -Wall #-}
module Haskell_IO (main) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Functor


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