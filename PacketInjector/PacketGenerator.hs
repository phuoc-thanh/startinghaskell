{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables , OverloadedStrings #-}

module PacketGenerator where

import Serializer
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C

tPacket :: ByteString
tPacket = C.append (hexSerialize $ C.length s) s
             where s = "store 8 2\NUL"

rankReward :: ByteString
rankReward = C.append (hexSerialize $ C.length s) s
                where s = "rankList reward 8\NUL"

activityItem :: ByteString -> ByteString
activityItem n = C.append (hexSerialize $ C.length s) s
                    where s = C.append "activity_items "
                            $ C.append n "\NUL"

activityReward :: ByteString -> ByteString
activityReward n = C.append (hexSerialize $ C.length s) s
                    where s = C.append "activity_reward "
                            $ C.append n " 0\NUL"                            

armyBase :: ByteString
armyBase = C.append (hexSerialize $ C.length s) s
                where s = "army base 0\NUL"

armyReward :: ByteString 
armyReward = C.append (hexSerialize $ C.length s) s
                where s = "army armyFoundation_award\NUL"

armyJoss :: ByteString 
armyJoss = C.append (hexSerialize $ C.length s) s
                where s = "army joss 2\NUL"

armyMisAward :: ByteString -> ByteString
armyMisAward n = C.append (hexSerialize $ C.length s) s
                    where s = C.append "army misaward "
                            $ C.append n "\NUL"

armyMisAccept :: ByteString -> ByteString
armyMisAccept n = C.append (hexSerialize $ C.length s) s
                    where s = C.append "army misaccept "
                            $ C.append n "\NUL" 

armyMisList :: ByteString
armyMisList = C.append (hexSerialize $ C.length s) s
                where s = "army mislist\NUL"

armyMisRefresh :: ByteString
armyMisRefresh = C.append (hexSerialize $ C.length s) s
                where s = "army misreflesh\NUL"                

armyMisSpdUp :: ByteString
armyMisSpdUp = C.append (hexSerialize $ C.length s) s
                 where s = "army misAcc\NUL"

armyExit :: ByteString
armyExit = C.append (hexSerialize $ C.length s) s
                where s = "army exit \NUL"

armyRequest :: ByteString -> ByteString
armyRequest idx = C.append (hexSerialize $ C.length s) s
                    where s = C.append "army request "
                            $ C.append idx "\NUL"    

armyReqList :: ByteString
armyReqList = C.append (hexSerialize $ C.length s) s
                where s = "army reqlist\NUL"

armyAgree :: ByteString -> ByteString
armyAgree idx = C.append (hexSerialize $ C.length s) s
        where s = C.append "army agree "
                $ C.append idx "\NUL"              

bet100 :: ByteString -> ByteString
bet100 idx = C.append (hexSerialize $ C.length s) s
                where s = C.append "crossserverwar betting "
                        $ C.append idx " 100\NUL"

bet1000 :: ByteString -> ByteString
bet1000 idx = C.append (hexSerialize $ C.length s) s
                where s = C.append "crossserverwar betting "
                         $ C.append idx " 1000\NUL"                        
iniHunt :: ByteString
iniHunt = C.append (hexSerialize $ C.length s) s
                where s = "teamHunt 1\NUL"

renewHunt :: ByteString       
renewHunt = C.append (hexSerialize $ C.length s) s
                where s = "teamHunt 15 0,1,2\NUL"

openRoom :: ByteString -> ByteString                
openRoom idx = C.append (hexSerialize $ C.length s) s
                where s = C.append "teamHunt 3 "
                        $ C.append  idx "\NUL"

goHunt :: ByteString -> ByteString                        
goHunt idx = C.append (hexSerialize $ C.length s) s
                where s = C.append "teamHunt 10 "
                        $ C.append  idx "\NUL" 

newUser :: ByteString -> ByteString
newUser uname = C.append (hexEnterSerialize $ C.length s) s
                where s = C.append "new_user "
                        $ C.append  uname " 1 2\NUL" 

chapter :: ByteString -> ByteString                
chapter c = C.append (hexSerialize $ C.length s) s
                where s = C.append "copy enter "
                        $ C.append  c " 1\NUL"                   
                                         
copyBlock :: ByteString
copyBlock = C.append (hexSerialize $ C.length s) s
                where s = "copy block\NUL"

copySwap :: ByteString -> ByteString
copySwap c = C.append (hexSerialize $ C.length s) s
                where s = C.append "copy swap "
                        $ C.append  c " 1 16\NUL"
                        
copySwap_ :: ByteString -> ByteString
copySwap_ c = C.append (hexSerialize $ C.length s) s
                where s = C.append "copy swap "
                        $ C.append  c " 1 20\NUL"                        

registeReward :: ByteString
registeReward = C.append (hexSerialize $ C.length s) s
    where s = "registeReward reward\NUL"

propUse :: ByteString -> ByteString -> ByteString
propUse c n = C.append (hexSerialize $ C.length s) s
                where s = C.append "prop_use " 
                        $ C.append c
                        $ C.append " "
                        $ C.append n "\NUL"
                
choiceCombat :: ByteString -> ByteString                
choiceCombat idx = C.append (hexSerialize $ C.length s) s
                    where s = C.append "choice_combat 0| "
                            $ C.append idx "#| -1\NUL"
campSelect :: ByteString -> ByteString
campSelect idc = C.append (hexSerialize $ C.length s) s
                where s = C.append "camp select "
                        $ C.append idc "\NUL"
   
--wboss 4 2
wboss :: ByteString
wboss = C.append (hexSerialize $ C.length s) s
        where s = "wboss 3 2\NUL"        
        
swallowCard :: ByteString -> ByteString
swallowCard c = C.append (hexSerialize $ C.length s) s
                where s = C.append "swallow_card 0 "
                        $ C.append c "\NUL"

shot :: ByteString
shot =  C.append (hexSerialize $ C.length s) s
    where s = "shot 1\NUL"                       