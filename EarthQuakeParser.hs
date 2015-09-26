module EarthQuakeParser where

-- {-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.ByteString as P
import qualified Data.Attoparsec.ByteString.Char8 as PC8
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as BS
import qualified Data.Time as T
import qualified Common as CM 
import Data.Conduit.Attoparsec (conduitParser, PositionRange)
import Control.Applicative ((<$>), (<*>), (<*), many, (<$), (<|>))
import Control.Monad.Trans(liftIO)
import Control.Monad.Catch
import qualified Data.Conduit.Binary as CB
import Data.Conduit 


query = "/home/blazej/Programowanie/EarthQake/query"

data EarthQuake = 
  EarthQuake{ time :: T.UTCTime
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
  show e = (show $ time e)++" "++(place e)
 

parseUTC :: String -> T.UTCTime
parseUTC = parseTimestamp 
  where
    parseTimestamp = 
      T.parseTimeOrError False T.defaultTimeLocale "%FT%T%QZ"
 
 
 
comma = PC8.char ','  
  
parseEarthQuake = do 
  time      <- parseUTC <$> parseWord
  _         <- comma
  latitude  <- parseWord
  _         <- comma
  longitude <- parseWord
  _         <- comma
  depth     <- parseWord
  _         <- comma
  mag       <- parseWord
  _         <- comma
  magType   <- parseWord
  _         <- comma
  nst       <- parseWord
  _         <- comma
  gap       <- parseWord
  _         <- comma
  dmin      <- parseWord
  _         <- comma
  rms       <- parseWord
  _         <- comma
  net       <- parseWord
  _         <- comma
  iD        <- parseWord
  _         <- comma
  updated   <- parseWord
  _         <- comma
  place     <- parseLocation
  _         <- comma
  typ       <- parseWord
  return $ EarthQuake time 
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
  
                    
  
   
parseEarthQuakeC :: MonadThrow m => Conduit BS.ByteString m (Either String EarthQuake)--(PositionRange, EarthQuake)  
parseEarthQuakeC =  awaitForever(\x->yield(PC8.parseOnly parseEarthQuake x))--conduitParser parseEarthQuake   
    
  
appSink :: CM.MonadResource m => Sink (Either String EarthQuake) m ()   --Sink (PositionRange, EarthQuake) m ()  
appSink = awaitForever (\x -> liftIO $ print x)
  
  
  
earthquakes :: CM.MonadResource m => FilePath -> m () 
earthquakes fn = do
  CB.sourceFile fn
  =$= CB.lines
  =$= CM.skip 1
  =$= parseEarthQuakeC
  $$ appSink
  
  
  
start :: IO()  
start = CM.runResourceT $ earthquakes query  
  
 
iso8601 :: T.UTCTime -> String
iso8601 = T.formatTime T.defaultTimeLocale "%FT%T%QZ"


noneOf cs = PC8.satisfy (\c -> not (elem c cs)) 
 
 
parseWord:: P.Parser String
parseWord = PC8.many' $ noneOf [',']

parseLocation:: P.Parser String
parseLocation = location <|> parseWord 
  where location = PC8.char '"' *> (PC8.many1 $ PC8.notChar '"') <*PC8.char '"' 
      
main2 = PC8.parseOnly parseEarthQuake $ 
   BC.pack "2015-09-18T15:59:42.800Z,15.2337,-45.9734,10,6,mwc,,31,13.337,1.12,us,us20003lc6,2015-09-19T01:57:01.000Z,\"Northern Mid-Atlantic, lala\", earthquake"

   
-- commaSep = (PC8.many1 $ PC8.notChar ',') `PC8.sepBy'` (PC8.char ',')            
   