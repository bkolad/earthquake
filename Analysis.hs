module Analysis where


import qualified Data.Conduit.List as CL
import qualified LHCDataParser as LHCP
import qualified Common as CM
import qualified Data.Conduit.Binary as CB
import qualified EarthQuakeParser as EQP
import Data.Conduit

import qualified Data.ByteString.Char8 as BC


posData = "/home/blazej/Programowanie/EarthQake/ROW_DATA/TIMBER_DATA_2011_01_01-2011_07_31.csv"

interval = 5*60*60 -- 5 houers


data TimeStatus = 
             Earlier 
           | InWindow 
           | Later    
           deriving (Show)
          
          
          
          
timeRange :: Double -> LHCP.POS_MEAN_H -> EQP.EarthQuake -> TimeStatus
timeRange interval pos eQ  =
  toRange dT (toRational interval) 
  where
    dT = toRational $ ( LHCP.time pos) `CM.diffUTCTime` (EQP.time eQ)
    
    toRange dt timeWindow
      | dt < 0.0 = Earlier  
      | dt <= timeWindow = InWindow
      | dt > timeWindow = Later
  
    



process :: 
  (CM.MonadThrow m, CM.MonadIO m)
  => [EQP.EarthQuake] 
  -> Conduit (CM.Perhaps LHCP.POS_MEAN_H) m (CM.Perhaps (EQP.EarthQuake, LHCP.POS_MEAN_H))    
process ls= 
  await >>= maybe (return()) (\x -> filterLHCData x ls)
    where 
      filterLHCData (Left a) ls = yield (Left a)
      filterLHCData (Right a) [] = return () 
      filterLHCData (Right a) (x:xs) = 
        do
           let k =  timeRange interval a x 
           case k of
             Earlier  -> process (x:xs)
             InWindow -> do 
               yield $ Right (x, a)
               process (x:xs)
             Later    -> process xs
      
                  
                    
                    
                  
lhcData :: 
  CM.MonadResource m 
  => FilePath 
  ->  [EQP.EarthQuake] -> m [CM.Perhaps (EQP.EarthQuake, LHCP.POS_MEAN_H)]   
lhcData fn ls = do 
  CB.sourceFile fn
  =$= CB.lines
  =$= CM.skip 3
  =$= (CM.parserC LHCP.parsePosition)
  =$= (process ls)
--  =$= CM.debug
  $$ CL.consume 
  
   
   
main = do
  ls <- EQP.earthQuakeList
  case ls of
    Left l -> print l
    Right r -> do k <- CM.runResourceT (sequence <$> lhcData posData (reverse r))
                  case k of 
                    Left b -> print b
                    Right t -> print (length t)
 
 
-- tR = (timeRange (60.0*60)) <$> LHCP.lhcD <*>  EQP.eqP     
 
 
--start :: IO (Either String [(EQP.EarthQuake, LHCP.POS_MEAN_H)])
--start = CM.runResourceT (sequence <$> lhcData posData)  