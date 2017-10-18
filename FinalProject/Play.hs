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
    pls <- cPls
    handler pls 0

dailyMis = do
    pls <- buffPls
    handler pls 1

dailyQ = do
    pls <- buffPls
    handler pls 2

groupOf :: Int -> [Player] -> [[Player]]          
groupOf _ [] = []           
groupOf n pls = take n pls : groupOf n (drop n pls)

handler :: [Player] -> Int -> IO ThreadId
handler pls idx = do
    cf <- getConfig    
    forkIO $ do
        conn <- joinWorld $ armyLead cf
        sendAll conn armyBase
        sendAll conn armyReward
        preload conn 32768
        C.putStrLn $ C.append (C.pack $ acc $ armyLead cf) " is ready"
        armyAgree_ conn
    threadDelay 5000000
    forkIO $ do
        forM_ (groupOf (armyGroup cf) pls) $ \gr -> do
            forkIO $ mHandler gr (C.pack $ armyId cf) idx
            threadDelay 90000000

mHandler :: [Player] -> ByteString -> Int -> IO ()
mHandler pls armyid idx = forM_ pls $ \p -> forkIO $ do
    tid <- myThreadId
    conn <- joinWorld p
    preload conn 32768
    activityRewards conn
    sendAll conn (armyRequest armyid)
    case idx of
        0 -> requestA p conn tid
        1 -> requestA_ p conn tid
        2 -> requestB p conn tid

armyAgree_ :: Socket -> IO ()            
armyAgree_ conn = do
    sendAll conn armyReqList
    threadDelay 800000
    msg <- recv conn 2048
    -- C.putStrLn $ encode msg
    -- C.putStrLn $ C.pack $ show $ filterR $ encode msg
    forM_ (map show $ filterR $ encode msg) $ \r -> sendAll conn $ armyAgree (C.pack r)
    threadDelay 6000000
    armyAgree_ conn

filterR :: ByteString -> [Integer]    
filterR msg
    |C.isInfixOf "8913" msg = map (hexDeserialize . C.take 8 . lastN 12 . C.pack) (init . split (endsWith "8913") $ C.unpack msg)
    |C.isInfixOf "8d13" msg = map (hexDeserialize . C.take 8 . lastN 12 . C.pack) (init . split (endsWith "8d13") $ C.unpack msg)
    |otherwise = map (hexDeserialize . C.take 8 . lastN 12 . C.pack) (init . split (endsWith "8e13") $ C.unpack msg)

requestA :: Player -> Socket -> ThreadId -> IO ()
requestA p conn t = waitfor "0300aa0801" (800000, 2048) conn $ do    
    sendAll conn armyBase
    sendAll conn armyReward
    sendAll conn armyJoss
    sendAll conn armyMisList
    C.putStrLn $ C.append (C.pack $ acc p) " joined army"
    missionGo 4 conn t

requestA_ :: Player -> Socket -> ThreadId -> IO ()
requestA_ p conn t = waitfor "0300aa0801" (800000, 2048) conn $ do
    sendAll conn armyBase
    sendAll conn armyReward
    sendAll conn armyJoss0
    sendAll conn armyExit
    recv conn 1024
    threadDelay 2000000
    close conn
    C.putStrLn $ C.append (C.pack $ acc p) ": job done!"
    killThread t

requestB :: Player -> Socket -> ThreadId -> IO ()
requestB p conn t = waitfor "0300aa0801" (800000, 2048) conn $ do
    forM_ (map show [0..4]) $ \x -> sendAll conn $ edenTreeGet (C.pack x)
    sendAll conn bless
    threadDelay 1600000
    sendAll conn armyBase
    sendAll conn armyReward
    sendAll conn armyJoss0
    sendAll conn armyExit
    recv conn 1024
    forM_ (map show [0..4]) $ \x -> sendAll conn $ edenTreeFinish (C.pack x)
    threadDelay 1600000
    forM_ (map show [0..4]) $ \x -> sendAll conn $ edenTreeGet (C.pack x)
    sendAll conn $ tavern "3"
    card <- waitforM "9200300001" (2000000, 1024) conn
    sendAll conn $ refineCard $ ref $ encode card
    sendAll conn $ tavern "4"
    threadDelay 1600000
    sendAll conn $ store1 "1022"
    sendAll conn $ store1 "1027"
    recv conn 1024
    forM_ (map show [0..4]) $ \x -> sendAll conn $ edenTreeFinish (C.pack x)
    threadDelay 1600000
    forM_ (map show [0..4]) $ \x -> sendAll conn $ edenTreeGet (C.pack x)
    sendAll conn $ edenTreeFinish "1"
    threadDelay 1600000
    sendAll conn $ edenTreeGet "1"
    sendAll conn iniHunt
    threadDelay 1600000
    forM_ (map show [0..9]) $ \x -> do
        sendAll conn $ goHunt "1"
        threadDelay 1600000
        sendAll conn copyBlock
    sendAll conn taskReward
    recv conn 1024
    threadDelay 2000000
    close conn
    C.putStrLn $ C.append (C.pack $ acc p) ": job done!"
    killThread t   
    
ref :: ByteString -> ByteString    
ref m = C.pack . show . hexDeserialize . C.drop 10 . C.take 16 . snd $ C.breakSubstring "9200300001" m

missionV :: (ByteString, Int) -> Int
missionV (m, i) = if C.isInfixOf m "0501dc05|0401b004" then i else 0

missionFilter :: ByteString -> [Int]
missionFilter msg = filter (>0) . map missionV $ zip refineM [1,2,3,4]
    where refineM = map (C.pack . drop 6) . split (startsWith "0000") . take 56 . drop 4 $ C.unpack msg

missionAccept :: Int -> [Int] -> Socket -> ThreadId -> IO ()    
missionAccept e m conn t
    | e == length m = do
        forM_ m $ \m0 -> sendAll conn $ armyMisAccept (C.pack $ show m0)
        threadDelay 1000000
        sendAll conn armyMisSpdUp
        threadDelay 1000000
        forM_ (map armyMisAward ["1","2","3","4"]) $ \p -> sendAll conn p
        sendAll conn armyMisList
        missionGo 4 conn t
    | 0 < length m = do
        forM_ m $ \m0 -> sendAll conn $ armyMisAccept (C.pack $ show m0)
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
        forM_ (map armyMisAward ["1","2","3","4"]) $ \p -> sendAll conn p
        threadDelay 2000000
        sendAll conn armyExit
        threadDelay 2000000
        close conn
        killThread t
    unless (C.isInfixOf "2300e904" $ encode msg) $ missionGo e conn t
    when (C.isInfixOf "2300e904" $ encode msg) $ do
        let m = missionFilter . snd . C.breakSubstring "2300e904" $ encode msg
        missionAccept e m conn t


activityRewards :: Socket -> IO ()
activityRewards conn = do
    sendAll conn $ activityItem "11091"
    forM_ ["86333","86334","86335","86336","86360","86361"] $ \n 
        -> sendAll conn $ activityReward n
    sendAll conn $ propUse "1031" "8"


----------------------------------------------------------


chapterOne = map chapter ["C01B03","C01B04","C01B05","C01B06","C01B07","C01B08"]
chapterTwo = map chapter ["C02B01","C02B02","C02B03","C02B04","C02B05","C02B06","C02B07","C02B08","C02B09","C02B10"]
-- repeatchapter x = take x . repeat $ (chapter "C03B01")

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
    goPtThree conn $ replicate 24 (chapter "C03B01")
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
    goPtFour conn $ replicate 21 (chapter "C03B01")
goPtThree conn chapter = waitfor "0d00440700" (3600000, 1024) conn $ do
    sendAll conn copyBlock
    sendAll conn $ head chapter
    goPtThree conn $ tail chapter

goPtFour :: Socket -> [ByteString] -> IO ()           
goPtFour conn [] = waitfor "0d00440700" (3600000, 1024) conn $ do
    cf <- getConfig
    sendAll conn $ campSelect $ C.pack (cloneCamp cf)    
    sendAll conn $ propUse "1022" "1"
    threadDelay 1600000
    sendAll conn $ copySwap "C03B01"
    goPtFive conn [copySwap "C03B01", copySwap_ "C03B01"]
goPtFour conn chapter = waitfor "0d00440700" (3600000, 1024) conn $ do
    sendAll conn copyBlock
    sendAll conn $ head chapter
    goPtFour conn $ tail chapter
                               
goPtFive :: Socket -> [ByteString] -> IO ()
goPtFive conn [] = waitfor "0900210300" (2400000, 1024) conn $ do
    threadDelay 2400000
    C.putStrLn "Done"
    close conn
goPtFive conn chapter = waitfor "0900210300" (2400000, 1024) conn $ do
    sendAll conn $ propUse "1022" "2"
    threadDelay 1600000
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
    return $ map ((++) (cloneKeyword cf) . show) [(idx+1)..(idx+n)]

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
    putStrLn $ u ++ " registered, leveling up.."
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
    forM_ pls $ \p -> forkIO $ do
        conn <- joinWorld p
        sendAll conn $ chapter "C01B01"
        goPtOne conn