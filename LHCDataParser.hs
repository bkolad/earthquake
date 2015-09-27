module LHCDataParser where

import qualified Common as CM
import qualified Data.ByteString.Char8 as BC
import Data.Time

data POS_MEAN_H = 
  POS_MEAN_H { time :: CM.UTCTime
             , value :: String
             } deriving (Show)
             
  
-- TODO if ERROR return FAILED from ATTOPARSEC, or P.PARSER UTC
        
parseUTC :: String -> CM.UTCTime
parseUTC = parseTimestamp 
  where
    parseTimestamp = 
      CM.parseTimeOrError False CM.defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"   
      
      
  
parsePosition :: CM.Parser POS_MEAN_H  
parsePosition = 
  do tm <- parseUTC <$> CM.parseWord
     _  <- CM.comma
     v  <- CM.parseWord
     return $ POS_MEAN_H tm v  
              
                           
               
main = CM.parseOnly parsePosition $ 
   BC.pack "2011-01-01 00:56:01.163,0"
   
   
  -- parseT = parseUTC "2011-01-01 00:56:02.44"   