module LHCDataParser where

import qualified Common as CM
import qualified Data.ByteString.Char8 as BC

data POS_MEAN_H = 
  POS_MEAN_H { time :: String--CM.UTCTime
             , value :: String
             } deriving (Show)
             
  
parsePosition :: CM.Parser POS_MEAN_H  
parsePosition = 
  do tm <- CM.parseWord
     _  <- CM.comma
     v  <- CM.parseWord
     return $ POS_MEAN_H tm v  
              
               
main = CM.parseOnly parsePosition $ 
   BC.pack "2011-01-01 00:56:01.163,0"                