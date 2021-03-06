module LHCDataParser where

import qualified Common as CM
import qualified Data.ByteString.Char8 as BC
import qualified Data.Conduit.List as CL
import Data.Conduit


data POS_MEAN_H = 
  POS_MEAN_H { time :: CM.UTCTime
             , value :: String
             } deriving (Show)
             

             
      
format = "%Y-%m-%d %H:%M:%S%Q"       
  
  
  
parsePosition :: CM.Parser POS_MEAN_H  
parsePosition = 
  do tm <- CM.parseUTC format CM.parseWord
     _  <- CM.comma
     v  <- CM.parseWord
     return $ POS_MEAN_H tm v  
            
                          
 
lhcD = CM.parseOnly parsePosition $ 
   BC.pack "2011-02-01 02:30:01.163,0"
   
   
encode :: POS_MEAN_H -> BC.ByteString
encode pm = BC.pack $ show (time pm) ++", "++ value pm
   
   
  -- parseT = parseUTC "2011-01-01 00:56:02.44"   