module EarthQuakeParser where

-- {-# LANGUAGE OverloadedStrings #-}

import qualified Common as CM 
import qualified Data.Conduit.List as CL
import Control.Applicative ((<|>))
import qualified Data.Conduit.Binary as CB
import qualified Data.ByteString.Char8 as BC
import Data.Conduit 


query = "/home/blazej/Programowanie/EarthQake/query"



data EarthQuake = 
  EarthQuake{ time :: CM.UTCTime
            , latitude :: String
            , longitude :: String  
            , depth :: String
            , mag :: String
            , magType :: String
            , nst :: String
            , gap :: String
            , dmin :: String
            , rms :: String
            , net :: String
            , id :: String
            , updated :: String
            , place :: String
            , typ :: String
            } 


            
instance Show EarthQuake where
  show e = "{"++(show $ time e)++"  ::  "++(place e) ++" }"
 
 
 
format = "%FT%T%QZ"
 
 
parseLocation:: CM.Parser String
parseLocation = location <|> CM.parseWord 
  where location = CM.char '"' *> (CM.many1 $ CM.notChar '"') <* CM.char '"' 
                      
 
 
  
parseEarthQuake :: CM.Parser EarthQuake  
parseEarthQuake = do 
  tm        <- CM.parseUTC format CM.parseWord
  _         <- CM.comma
  latitude  <- CM.parseWord
  _         <- CM.comma
  longitude <- CM.parseWord
  _         <- CM.comma
  depth     <- CM.parseWord
  _         <- CM.comma
  mag       <- CM.parseWord
  _         <- CM.comma
  magType   <- CM.parseWord
  _         <- CM.comma
  nst       <- CM.parseWord
  _         <- CM.comma
  gap       <- CM.parseWord
  _         <- CM.comma
  dmin      <- CM.parseWord
  _         <- CM.comma
  rms       <- CM.parseWord
  _         <- CM.comma
  net       <- CM.parseWord
  _         <- CM.comma
  iD        <- CM.parseWord
  _         <- CM.comma
  updated   <- CM.parseWord
  _         <- CM.comma
  place     <- parseLocation
  _         <- CM.comma
  typ       <- CM.parseWord
  return $ EarthQuake tm
                      latitude 
                      longitude
                      depth
                      mag
                      magType
                      nst
                      gap
                      dmin
                      rms
                      net
                      iD
                      updated
                      place
                      typ 
                      
                         
  
earthquakes :: 
  CM.MonadResource m 
  => FilePath 
  -> m [CM.Perhaps EarthQuake]
earthquakes fn = do
  CB.sourceFile fn
  =$= CB.lines
  =$= CM.skip 1
  =$= (CM.parserC parseEarthQuake)
  =$= CM.cutOffC
  $$ CL.consume 
  
  
  
earthQuakeList :: IO (Either String [EarthQuake])
earthQuakeList = CM.runResourceT (sequence <$> earthquakes query)  
  
       
      
      
main = CM.parseOnly parseEarthQuake $ 
   BC.pack "2015-09-18T15:59:42.800Z,15.2337,-45.9734,10,6,mwc,,31,13.337,1.12,us,us20003lc6,2015-09-19T01:57:01.000Z,\"Northern Mid-Atlantic, lala\", earthquake"

   
   
   