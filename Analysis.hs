module Analysis where


import qualified Data.Conduit.List as CL
import qualified LHCDataParser as LHCP
import qualified Common as CM
import qualified Data.Conduit.Binary as CB
import qualified EarthQuakeParser as EQP
import Data.Conduit

import qualified Data.ByteString.Char8 as BC


posData = "/home/blazej/Programowanie/EarthQake/ROW_DATA/TIMBER_DATA_2011_01_01-2011_07_31.csv"



process :: 
  (CM.MonadThrow m, CM.MonadIO m)
  => [EQP.EarthQuake] -> Conduit (CM.Perhaps LHCP.POS_MEAN_H) m (CM.Perhaps (EQP.EarthQuake, LHCP.POS_MEAN_H))    
process ls= 
  await >>= maybe (return()) (\x -> filterLHCData x ls)
    where 
      filterLHCData (Left a) ls = yield (Left a)
      filterLHCData (Right a) [] = return () 
   {--   filterLHCData (Right a) (x:xs) = 
        do
           let k =  inWindow a x 
           case k of
             Earlier -> process (x:xs)
             InWindow -> do 
               yield $ Right (x, a)
               processs (x:xs)
             Later -> process xs
      
      --}              
                    
                    
data Range = Earlier 
           | InWindow 
           | Later                     
                    
                    
timeRange:: EQP.EarthQuake -> LHCP.POS_MEAN_H -> CM.NominalDiffTime
timeRange eQ pos = (EQP.time eQ) `CM.diffUTCTime` ( LHCP.time pos)
                  
                  
format = "%Y-%m-%d %H:%M:%S%Q"  


parseUTC = CM.parseUTC format CM.parseWord
                  
t1 = CM.parseOnly parseUTC $ BC.pack "2011-01-01 00:52:01.163"

t2 = CM.parseOnly parseUTC $ BC.pack "2011-01-01 00:51:01.163"                  
                  

--dff :: Either String Double                  
dff = (fromRational . toRational) <$> (CM.diffUTCTime <$> t1 <*> t2)
                  

                  
-- dff2 = ((fromRational . toRational . CM.diffUTCTime) <$> t1 <*> t2)
                  
                  
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
  =$= CM.debug
  $$ CL.consume 
  
   
   
main = do
  ls <- EQP.earthQuakeList
  case ls of
    Left l -> print l
    Right r -> print r
   
 
--start :: IO (Either String [LHCP.POS_MEAN_H])
--start = CM.runResourceT (sequence <$> lhcData posData)  