{-# LANGUAGE OverloadedStrings #-}

module Play where

import Authenticator
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

main = do 
    cf <- getConfig    
    forkIO $ do
        conn <- joinWorld $ armyLead cf
        sendAll conn armyBase
        armyAgree_ conn $ C.pack $ cloneKeyword cf
    threadDelay 5000000
    forkIO $ do
        pls <- cPls
        forM_ (groupOf (armyGroup cf) pls) $ \gr -> do
            forkIO $ do
                mHandler gr (C.pack $ armyId cf)
            threadDelay 120000000

groupOf :: Int -> [Player] -> [[Player]]          
groupOf _ [] = []           
groupOf n pls = (take n pls) : groupOf n (drop n pls)

mHandler :: [Player] -> ByteString -> IO ()
mHandler pls armyid = do
    forM_ pls $ \p -> do
        forkIO $ do
            tid <- myThreadId
            conn <- joinWorld p
            sendAll conn (armyRequest armyid)
            requestA conn tid

armyAgree_ :: Socket -> ByteString -> IO ()            
armyAgree_ conn keyword = do
    sendAll conn armyReqList
    threadDelay 800000
    msg <- recv conn 2048
    unless (C.isInfixOf keyword msg) $ armyAgree_ conn keyword
    when (C.isInfixOf keyword msg) $ do
        pl <- getClone . C.unpack . C.take (4 + C.length keyword) . snd $ C.breakSubstring keyword msg
        sendAll conn $ armyAgree (C.pack $ chNumber pl)
        armyAgree_ conn keyword

armyMis :: IO ()                  
armyMis  = do 
    bUsers <- buffPls
    forM_ bUsers $ \u -> do
        forkIO $ do
            tid <- myThreadId
            conn <- joinWorld u
            sendAll conn armyBase
            sendAll conn armyReward
            threadDelay 2000000
            C.putStrLn "Done"

armyMis_ :: IO ()                  
armyMis_  = do
    cf <- getConfig
    bUsers <- buffPls
    let armyid = C.pack $ armyId cf
    forM_ bUsers $ \u -> do
        forkIO $ do
            tid <- myThreadId
            conn <- joinWorld u
            sendAll conn (armyRequest armyid)
            requestA_ conn tid                  

requestA :: Socket -> ThreadId -> IO ()
requestA conn t = do 
    threadDelay 800000
    msg <- recv conn 2048
    unless (C.isInfixOf "0300aa0801" $ encode msg) $ requestA conn t
    when (C.isInfixOf "0300aa0801" $ encode msg) $ do
        sendAll conn armyBase
        sendAll conn armyReward
        sendAll conn armyJoss
        sendAll conn armyMisList
        missionGo 4 conn t

requestA_ :: Socket -> ThreadId -> IO ()
requestA_ conn t = do 
    threadDelay 800000
    msg <- recv conn 2048
    unless (C.isInfixOf "0300aa0801" $ encode msg) $ requestA conn t
    when (C.isInfixOf "0300aa0801" $ encode msg) $ do
        sendAll conn armyBase
        sendAll conn armyReward
        threadDelay 2000000
        sendAll conn armyExit
        threadDelay 2000000
        close conn
        killThread t
        C.putStrLn "DONE!"                        
                    
missionV :: (ByteString, Int) -> Int
missionV (m, i) = if (C.isInfixOf m "0501dc05|0401b004") then i else 0

missionFilter :: ByteString -> [Int]
missionFilter msg = filter (>0) . map missionV $ zip refineM [1,2,3,4]
    where refineM = map (C.pack . drop 6) . split (startsWith "0000") . take 56 . drop 4 $ C.unpack msg

missionAccept :: Int -> [Int] -> Socket -> ThreadId -> IO ()    
missionAccept e m conn t
    | e == length m = do
        forM_ m $ \m0 -> do
            sendAll conn $ armyMisAccept (C.pack $ show m0)
        threadDelay 1000000
        sendAll conn armyMisSpdUp
        threadDelay 1000000
        forM_ (map (armyMisAward) ["1","2","3","4"]) $ \p -> do
            sendAll conn p
        sendAll conn armyMisList
        missionGo 4 conn t
    | 0 < length m = do
        forM_ m $ \m0 -> do
            sendAll conn $ armyMisAccept (C.pack $ show m0)
        threadDelay 1000000
        sendAll conn armyMisRefresh
        sendAll conn armyMisList
        missionGo (e - length m) conn t 
    | otherwise = do
        sendAll conn armyMisRefresh
        sendAll conn armyMisList
        missionGo e conn t

missionGo :: Int -> Socket -> ThreadId -> IO ()
missionGo e conn t = do
    threadDelay 1000000
    msg <- recv conn 2048
    when (C.isInfixOf "0300a12905" $ encode msg) $ do
        sendAll conn armyMisSpdUp
        threadDelay 1000000
        forM_ (map (armyMisAward) ["1","2","3","4"]) $ \p -> do
            sendAll conn p
        threadDelay 2000000
        sendAll conn armyExit
        threadDelay 2000000
        close conn
        C.putStrLn "exit Army!"
        killThread t
    unless (C.isInfixOf "2300e904" $ encode msg) $ missionGo e conn t
    when (C.isInfixOf "2300e904" $ encode msg) $ do
        let m = missionFilter . snd . C.breakSubstring "2300e904" $ encode msg
        missionAccept e m conn t


chapterOne = map (chapter) ["C01B03","C01B04","C01B05","C01B06","C01B07","C01B08"]
chapterTwo = map (chapter) ["C02B01","C02B02","C02B03","C02B04","C02B05","C02B06","C02B07","C02B08","C02B09","C02B10"]
repeatchapter x = take x . repeat $ (chapter "C03B01")

lvlup pl = do conn <- joinWorld pl
              sendAll conn $ chapter "C01B01"
              goPtOne conn

goPtOne :: Socket -> IO ()              
goPtOne conn = do 
    threadDelay 2000000
    msg <- recv conn 1024
    unless (C.isInfixOf "0090130000" $ encode msg) $ goPtOne conn
    when (C.isInfixOf "0090130000" $ encode msg) $ do
        sendAll conn $ choiceCombat (C.pack . show . eCombat $ fst $ C.breakSubstring "0090130000" $ encode msg)
        threadDelay 2000000
        sendAll conn copyBlock
        sendAll conn $ chapter "C01B02"
        goPtTwo conn (chapterOne ++ chapterTwo)

goPtTwo :: Socket -> [ByteString] -> IO ()           
goPtTwo conn [] = do sendAll conn $ chapter "C03B01"
                     goPtThree conn $ repeatchapter 24
goPtTwo conn chapter = do  threadDelay 4000000
                           msg <- recv conn 2048
                           unless (C.isInfixOf "0d00440700" $ encode msg) $ goPtTwo conn chapter
                           when (C.isInfixOf "0d00440700" $ encode msg) $ do
                               sendAll conn copyBlock
                               sendAll conn $ head chapter
                               goPtTwo conn $ tail chapter

goPtThree :: Socket -> [ByteString] -> IO ()           
goPtThree conn [] = do sendAll conn registeReward
                       sendAll conn useEnergy
                       threadDelay 200000
                       sendAll conn $ chapter "C03B01"
                       goPtFour conn $ repeatchapter 24
goPtThree conn chapter = do threadDelay 3000000
                            msg <- recv conn 2048
                            unless (C.isInfixOf "0d00440700" $ encode msg) $ goPtThree conn chapter
                            when (C.isInfixOf "0d00440700" $ encode msg) $ do
                              sendAll conn copyBlock
                              sendAll conn $ head chapter
                              goPtThree conn $ tail chapter

goPtFour :: Socket -> [ByteString] -> IO ()           
goPtFour conn [] = do sendAll conn useEnergy
                      threadDelay 200000
                      sendAll conn $ (copySwap "C03B01")
                      goPtFive conn $ [(copySwap "C03B01"), (copySwap "C03B01")]
goPtFour conn chapter = do threadDelay 3000000
                           msg <- recv conn 2048
                           unless (C.isInfixOf "0d00440700" $ encode msg) $ goPtFour conn chapter
                           when (C.isInfixOf "0d00440700" $ encode msg) $ do
                               sendAll conn copyBlock
                               sendAll conn $ head chapter
                               goPtFour conn $ tail chapter
                               
goPtFive :: Socket -> [ByteString] -> IO ()
goPtFive conn [] = do cf <- getConfig
                      sendAll conn $ campSelect $ C.pack (cloneCamp cf)
                      sendAll conn $ copySwap_ "C03B01"
                      threadDelay 2000000
                      C.putStrLn "Done"
                      close conn
goPtFive conn chapter = do threadDelay 1000000
                           msg <- recv conn 2048
                           unless (C.isInfixOf "0900210300" $ encode msg) $ goPtFive conn chapter
                           when (C.isInfixOf "0900210300" $ encode msg) $ do
                               sendAll conn useEnergy
                               sendAll conn $ head chapter
                               goPtFive conn $ tail chapter

lastN :: Int -> ByteString -> ByteString
lastN n xs = C.drop (C.length xs - n) xs

eCombat :: ByteString -> Integer
eCombat = hexDeserialize . lastN 6

regString :: String -> [Int] -> [String]
regString p n = zipWith (++) (repeat p) (map show n)

massReg :: String -> [Int] -> IO ()
massReg p n = do
    cf <- getConfig
    let rString = regString p n
    forM_ rString $ \u -> do
            cChar u (defaultPass cf) (cloneServer cf) 

cChar u p s = do 
    (conn, res) <- reg u p s
    sendAll conn $ newUser (C.pack u)
    msg <- recv conn 256
    let chN = show . newChNumber $ encode msg
    sendAll conn $ enterW (C.pack $ uid res) (C.pack chN)
    pls <- cPls
    let pl = Player u (uid res) (opname res) s (displayNovice res) 
                    (create_time res) (key res) chN (amount res)    
    appendJSON "Clone.json" (pl:pls)
    threadDelay 2000000
    close conn

lvlup_ = do pl <- cPls
            forM_ pl $ \u -> do
                forkIO $ do
                    lvlup u