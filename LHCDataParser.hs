module LHCDataParser where

import qualified Common as CM
import qualified Data.ByteString.Char8 as BC
import Data.Time

import Data.Attoparsec.Internal.Types

import qualified Data.ByteString as BS


data POS_MEAN_H = 
  POS_MEAN_H { time :: CM.UTCTime
             , value :: String
             } deriving (Show)
             

             

      
format = "%Y-%m-%d %H:%M:%S%Q"       
  
parsePosition :: CM.Parser POS_MEAN_H  
parsePosition = 
  do tm <- CM.parseWord 
     case CM.parseUTC format tm of
       Nothing -> fail tm
       Just x -> do
         _  <- CM.comma
         v  <- CM.parseWord
         return $ POS_MEAN_H x v  
              
                           
    
    
main = CM.parseOnly parsePosition $ 
   BC.pack "2011-01-01 00:56:01.163,0"
   
   
  -- parseT = parseUTC "2011-01-01 00:56:02.44"   