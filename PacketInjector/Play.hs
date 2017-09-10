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
            activityRewards conn
            recv conn 2048
            threadDelay 1600000
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

requestA :: Socket -> ThreadId -> IO ()
requestA conn t = waitfor "0300aa0801" (800000, 2048) conn $ do    
    sendAll conn armyBase
    sendAll conn armyReward
    sendAll conn armyJoss
    sendAll conn armyMisList
    missionGo 4 conn t

dailyMis :: IO ()                  
dailyMis  = do
    cf <- getConfig
    pls <- buffPls
    let armyid = C.pack $ armyId cf
    forM_ pls $ \u -> do
        forkIO $ do
            tid <- myThreadId
            conn <- joinWorld u
            sendNTimes 3 conn shot
            sendAll conn (armyRequest armyid)
            requestA_ conn tid                  

requestA_ :: Socket -> ThreadId -> IO ()
requestA_ conn t = waitfor "0300aa0801" (800000, 2048) conn $ do
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


activityRewards :: Socket -> IO ()
activityRewards conn = do
    sendAll conn $ activityItem "11091"
    forM_ ["86333","86334","86335","86336","86360","86361"] $ \n -> do
        sendAll conn $ activityReward n
    sendAll conn $ propUse "1031" "8"


----------------------------------------------------------


chapterOne = map (chapter) ["C01B03","C01B04","C01B05","C01B06","C01B07","C01B08"]
chapterTwo = map (chapter) ["C02B01","C02B02","C02B03","C02B04","C02B05","C02B06","C02B07","C02B08","C02B09","C02B10"]
repeatchapter x = take x . repeat $ (chapter "C03B01")

goPtOne :: Socket -> IO ()              
goPtOne conn = do
    msg <- waitforM "0090130000" (2000000, 1024) conn
    sendAll conn $ choiceCombat (C.pack . show . eCombat $ fst $ C.breakSubstring "0090130000" $ encode msg)
    threadDelay 2000000
    sendAll conn copyBlock
    sendAll conn $ chapter "C01B02"
    goPtTwo conn (chapterOne ++ chapterTwo)

goPtTwo :: Socket -> [ByteString] -> IO ()           
goPtTwo conn [] = do 
    sendAll conn $ chapter "C03B01"
    goPtThree conn $ repeatchapter 24
goPtTwo conn chapter = waitfor "0d00440700" (4000000, 2048) conn $ do
    sendAll conn copyBlock
    sendAll conn $ head chapter
    goPtTwo conn $ tail chapter

goPtThree :: Socket -> [ByteString] -> IO ()           
goPtThree conn [] = do
    sendAll conn registeReward    
    sendAll conn $ propUse "1022" "2"
    threadDelay 1600000
    sendAll conn $ chapter "C03B01"
    goPtFour conn $ repeatchapter 20
goPtThree conn chapter = waitfor "0d00440700" (3600000, 2048) conn $ do
    sendAll conn copyBlock
    sendAll conn $ head chapter
    goPtThree conn $ tail chapter

goPtFour :: Socket -> [ByteString] -> IO ()           
goPtFour conn [] = waitfor "0d00440700" (3600000, 2048) conn $ do
    cf <- getConfig
    sendAll conn $ campSelect $ C.pack (cloneCamp cf)    
    sendAll conn $ propUse "1022" "1"
    threadDelay 1600000
    sendAll conn $ (copySwap "C03B01")
    goPtFive conn $ [(copySwap "C03B01"), (copySwap_ "C03B01")]
goPtFour conn chapter = waitfor "0d00440700" (3600000, 2048) conn $ do
    sendAll conn copyBlock
    sendAll conn $ head chapter
    goPtFour conn $ tail chapter
                               
goPtFive :: Socket -> [ByteString] -> IO ()
goPtFive conn [] = waitfor "0900210300" (3200000, 2048) conn $ do
    threadDelay 2400000
    C.putStrLn "Done"
    close conn
goPtFive conn chapter = waitfor "0900210300" (3200000, 2048) conn $ do
    sendAll conn $ propUse "1022" "2"
    sendAll conn $ head chapter
    goPtFive conn $ tail chapter

lastN :: Int -> ByteString -> ByteString
lastN n xs = C.drop (C.length xs - n) xs

eCombat :: ByteString -> Integer
eCombat = hexDeserialize . lastN 6

nextP :: Int -> PreConfig -> IO [String]
nextP n cf = do
    pls <- cPls
    let idx = read $ drop (length $ cloneKeyword cf) (acc $ head pls)
    return $ zipWith (++) (repeat $ cloneKeyword cf) (map show [(idx+1)..(idx+n)])

regA :: String -> String -> String -> IO (Player, Socket)    
regA u p s = do 
    (conn, res) <- reg u p s
    sendAll conn $ newUser (C.pack u)
    msg <- recv conn 256
    let chN = show . newChNumber $ encode msg
    sendAll conn $ enterW (C.pack $ uid res) (C.pack chN)
    let pl = Player u (uid res) (opname res) s (displayNovice res) 
                    (create_time res) (key res) chN (amount res)    
    threadDelay 1600000
    putStrLn $ u ++ " is registered, leveling up.."
    return (pl, conn)

add :: Int -> IO ()
add n = do
    cf <- getConfig
    np <- nextP n cf
    forM_ np $ \u -> do
        (pl, conn) <- regA u (defaultPass cf) (cloneServer cf)
        pls <- cPls
        appendJSON "Clone.json" (pl:pls)
        forkIO $ do
            sendAll conn $ chapter "C01B01"
            goPtOne conn

lvlup :: IO ()      
lvlup = do
    pls <- cPls
    forM_ pls $ \p -> do
        forkIO $ do
            conn <- joinWorld p
            sendAll conn $ chapter "C01B01"
            goPtOne conn