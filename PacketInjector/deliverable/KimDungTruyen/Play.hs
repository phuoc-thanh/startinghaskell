{-# LANGUAGE OverloadedStrings #-}

module Play where

import Injector
import PacketGenerator
import Parser hiding (encode, decode)
import Serializer (hexDeserialize)
import qualified Data.ByteString.Char8 as C
import Data.ByteString (ByteString)
import Data.ByteString.Base16
import Data.List.Split
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (recv, sendAll)
import Control.Monad
import Control.Concurrent

          
sendP :: String -> Integer -> IO ()
sendP uname n = do user <- getPlayer uname
                   conn <- joinWorld user
                   sendNTimes n conn tPacket
                   close conn

armyMis_ :: IO ()                  
armyMis_ = do clone <- cPls
              forM_ clone $ \u -> do
                forkIO $ do
                    tid <- myThreadId
                    conn <- joinWorld u
                    sendAll conn (armyRequest "102000000")
                    requestA conn tid

armyMis :: IO ()                  
armyMis  = do bUsers <- cPls
              forM_ bUsers $ \u -> do
                forkIO $ do
                    tid <- myThreadId
                    conn <- joinWorld u
                    sendAll conn armyBase
                    sendAll conn armyReward
                    sendAll conn armyMisList
                    listenA' conn tid

requestA :: Socket -> ThreadId -> IO ()
requestA conn t = do threadDelay 2000000
                     msg <- recv conn 2048
                     unless (C.isInfixOf "0300aa0801" $ encode msg) $ requestA conn t
                     when (C.isInfixOf "0300aa0801" $ encode msg) $ do
                         sendAll conn armyReward
                         sendAll conn armyMisList
                         listenA conn t
                    

listenA :: Socket -> ThreadId -> IO ()              
listenA conn t = do threadDelay 2000000
                    msg <- recv conn 2048
                    when (C.isInfixOf "0300a12905" $ encode msg) $ do
                        sendAll conn armyExit
                        threadDelay 1000000
                        C.putStrLn "exit Army!"
                        close conn
                        killThread t
                    unless (C.isInfixOf "2300e904" $ encode msg) $ listenA conn t
                    when (C.isInfixOf "2300e904" $ encode msg) $ do
                        sendAll conn armyBase
                        threadDelay 200000
                        sendAll conn armyJoss
                        forM_ (map (armyMisAward) ["1","2","3","4"]) $ \p -> do
                            sendAll conn p
                        threadDelay 1000000
                        recv conn 2048
                        forM_ (map (armyMisAccept) ["1","2","3","4"]) $ \p -> do
                            sendAll conn p
                        threadDelay 2000000
                        sendAll conn armyMisSpdUp
                        sendAll conn armyMisList
                        listenA conn t
                    
listenA' :: Socket -> ThreadId -> IO ()              
listenA' conn t = do    threadDelay 200000
                        msg <- recv conn 2048
                        when (C.isInfixOf "0300a12905" $ encode msg) $ do
                            C.putStrLn "done!"
                            close conn
                            killThread t
                        unless (C.isInfixOf "2300e904" $ encode msg) $ listenA' conn t
                        when (C.isInfixOf "2300e904" $ encode msg) $ do
                            forM_ (map (armyMisAward) ["1","2","3","4"]) $ \p -> do
                                sendAll conn p
                            threadDelay 1000000
                            forM_ (map (armyMisAccept) ["1","2","3","4"]) $ \p -> do
                                sendAll conn p
                            threadDelay 2000000
                            sendAll conn armyMisSpdUp
                            sendAll conn armyMisList
                            listenA' conn t

chapterOne = map (chapter) ["C01B03","C01B04","C01B05","C01B06","C01B07","C01B08"]
chapterTwo = map (chapter) ["C02B01","C02B02","C02B03","C02B04","C02B05","C02B06","C02B07","C02B08","C02B09","C02B10"]
chapterThree = take 20 . repeat $ (chapter "C03B01")
xchapterOne = map (xchapter) ["XB01B02","XB01B03","XB01B04","XB01B05"]

lvlup pl = do conn <- joinWorld pl
              sendAll conn $ chapter "C01B01"
              combat conn

goPtOne :: Socket -> [ByteString] -> IO ()           
goPtOne conn [] = do threadDelay 2000000
                     sendAll conn $ xchapter "XB01B01"
                     goPtTwo conn xchapterOne
goPtOne conn chapter = do  threadDelay 4000000
                           msg <- recv conn 2048
                           unless (C.isInfixOf "0d00440700" $ encode msg) $ goPtOne conn chapter
                           when (C.isInfixOf "0d00440700" $ encode msg) $ do
                               sendAll conn copyBlock
                               sendAll conn $ head chapter
                               goPtOne conn $ tail chapter

goPtTwo :: Socket -> [ByteString] -> IO ()           
goPtTwo conn [] = do threadDelay 2000000
                     sendAll conn $ chapter "C03B01"
                     goPtThree conn chapterThree
goPtTwo conn chapter = do  threadDelay 4000000
                           msg <- recv conn 2048
                           unless (C.isInfixOf "0d00440700" $ encode msg) $ goPtTwo conn chapter
                           when (C.isInfixOf "0d00440700" $ encode msg) $ do
                               sendAll conn copyBlock
                               sendAll conn $ head chapter
                               goPtTwo conn $ tail chapter

goPtThree :: Socket -> [ByteString] -> IO ()           
goPtThree conn [] = do threadDelay 2000000
                       sendAll conn firstReward
                       threadDelay 2000000
                       sendAll conn useEnergy
                       threadDelay 2000000
                       sendAll conn $ chapter "C03B01"
                       goPtFour conn chapterThree
goPtThree conn chapter = do threadDelay 3000000
                            msg <- recv conn 2048
                            unless (C.isInfixOf "0d00440700" $ encode msg) $ goPtThree conn chapter
                            when (C.isInfixOf "0d00440700" $ encode msg) $ do
                              sendAll conn copyBlock
                              sendAll conn $ head chapter
                              goPtThree conn $ tail chapter

goPtFour :: Socket -> [ByteString] -> IO ()           
goPtFour conn [] = do threadDelay 2000000
                      C.putStrLn "done!"
                      close conn
goPtFour conn chapter = do threadDelay 3000000
                           msg <- recv conn 2048
                           unless (C.isInfixOf "0d00440700" $ encode msg) $ goPtFour conn chapter
                           when (C.isInfixOf "0d00440700" $ encode msg) $ do
                               sendAll conn copyBlock
                               sendAll conn $ head chapter
                               goPtFour conn $ tail chapter                          

lastN :: Int -> ByteString -> ByteString
lastN n xs = C.drop (C.length xs - n) xs

eCombat :: ByteString -> Integer
eCombat = hexDeserialize . lastN 6

combat conn = do threadDelay 2000000
                 msg <- recv conn 1024
                 unless (C.isInfixOf "0090130000" $ encode msg) $ combat conn
                 when (C.isInfixOf "0090130000" $ encode msg) $ do
                    sendAll conn $ choiceCombat (C.pack . show . eCombat $ fst $ C.breakSubstring "0090130000" $ encode msg)
                    threadDelay 2000000
                    sendAll conn copyBlock
                    sendAll conn $ chapter "C01B02"
                    goPtOne conn (chapterOne ++ chapterTwo)

regString = zipWith (++) (repeat "revive") (map show [11..22])
                
massReg (x:xs) = do cChar x "123456" "1"
                    massReg xs              

cChar u p s = do 
    pl <- reg u p s
    pls <- cPls
    appendJSON "Clone.json" (pl:pls)

lvlup_ = do pl <- cPls
            forM_ pl $ \u -> do
                forkIO $ do
                    lvlup u
                    --75/76/80