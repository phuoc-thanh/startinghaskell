{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables , OverloadedStrings #-}

module PacketGenerator where

import Serializer
import qualified Data.ByteString.Char8 as C

rankReward :: C.ByteString
rankReward = C.append (hexSerialize $ C.length s) s
                where s = "rankList reward 5\NUL"

armyReward :: C.ByteString 
armyReward = C.append (hexSerialize $ C.length s) s
                where s = "army armyFoundation_award\NUL"

                --
armyMisAward :: C.ByteString -> C.ByteString
armyMisAward n = C.append (hexSerialize $ C.length s) s
                    where s = C.append "army misaward "
                            $ C.append n "\NUL"

armyMisAccept :: C.ByteString -> C.ByteString
armyMisAccept n = C.append (hexSerialize $ C.length s) s
                    where s = C.append "army misaccept "
                            $ C.append n "\NUL"

tPacket :: C.ByteString
tPacket = C.append (hexSerialize $ C.length s) s
             where s = "store 8 2\NUL"

bet100 :: C.ByteString -> C.ByteString
bet100 idx = C.append (hexSerialize $ C.length s) s
                where s = C.append "crossserverwar betting "
                        $ C.append idx " 100\NUL"

bet1000 :: C.ByteString -> C.ByteString
bet1000 idx = C.append (hexSerialize $ C.length s) s
                 where s = C.append "crossserverwar betting "
                         $ C.append idx " 1000\NUL"                        
-- VTM ZM44, TDLT ZM51, HT ZM45
-- Hunt: teamHunt 4 --> teamHunt 3 i (i =  0|1|2)
huntRefresh = C.append (hexSerialize $ C.length s) s
                where s = "teamHunt 15 0,1,2\NUL"
openRoom i  = C.append (hexSerialize $ C.length s) s
                where s = C.append "teamHunt 3 "
                        $ C.append  i "\NUL"

-- copyC01B01 = C.append (hexSerialize $ C.length s) s
--                 where s = "copy enter C01B01 1\NUL"

-- copyC01B02 = C.append (hexSerialize $ C.length s) s
--                 where s = "copy enter C01B02 1\NUL"                
                
-- copyBlock = C.append (hexSerialize $ C.length s) s
--                 where s = "copy block\NUL"
                
-- choiceCombat0 = C.append (hexSerialize $ C.length s) s
--                     where s = "choice_combat 0| 344716#| -1\NUL"

-- clientGuide1 = C.append (hexSerialize $ C.length s) s
--                 where s = "client guide1\NUL"                    
                    