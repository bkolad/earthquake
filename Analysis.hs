module Analysis where


import qualified Data.Conduit.List as CL
import qualified LHCDataParser as LHCP
import qualified Common as CM
import qualified Data.Conduit.Binary as CB
import qualified EarthQuakeParser as EQP
import qualified System.IO as SIO
import qualified Control.Exception as E
import Data.Conduit

import qualified Data.ByteString.Char8 as BC


posData = "/home/blazej/Programowanie/EarthQake/ROW_DATA/TIMBER_DATA_2011_01_01-2011_07_31.csv"

interval = 5*60*60 -- 5 houers

output = "/home/blazej/Programowanie/EarthQake/FILTERED DATA/"

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
  -> Conduit (CM.Perhaps LHCP.POS_MEAN_H) m (EQP.EarthQuake, LHCP.POS_MEAN_H)    
process ls= 
  await >>= maybe (return()) (\x -> filterLHCData x ls)
    where 
      filterLHCData (Left a) _ = 
        CM.liftIO $ print ("******* ERROR ************ " ++ show a) 
        
      filterLHCData (Right a) [] = 
        return ()
        
      filterLHCData (Right a) (x:xs) = 
        do
           let k =  timeRange interval a x 
           case k of
             Earlier  -> process (x:xs)
             InWindow -> do 
               yield $ (x, a)
               process (x:xs)
             Later    -> filterLHCData (Right a)  xs
      
        
        
coll :: 
  Monad m => 
  Maybe(EQP.EarthQuake, [LHCP.POS_MEAN_H]) 
  -> Conduit (EQP.EarthQuake, LHCP.POS_MEAN_H) m (EQP.EarthQuake, [LHCP.POS_MEAN_H])             
coll st =   
  await >>= maybe (return()) (\x -> fun x st)
  where
    fun (e,l) Nothing = coll $ Just (e, [l]) 
    fun (e,l) (Just(a, b)) | e == a = coll $ Just(a, l:b)
    fun (e,l) (Just(a, b)) | e /= a = do
      yield (a, reverse b)
      coll $ Just (e, [l])
    
  
                  
saveToFile :: CM.MonadResource m
  =>  Sink (EQP.EarthQuake, [LHCP.POS_MEAN_H])  m ()     
saveToFile = do
  xM <- await
  case xM of 
    Nothing -> return ()
    Just (x,ls) -> (CM.liftIO $ CM.runResourceT $
      (CL.sourceList ls)
      =$= (CL.map (\_ -> BC.pack "aa"))
      $$ (CB.sinkFile (output ++ (show x)))) >> saveToFile
      
           
           
           
           
           
lhcData :: 
  CM.MonadResource m 
  => FilePath 
  ->  [EQP.EarthQuake] -> m ()--[(EQP.EarthQuake, [LHCP.POS_MEAN_H])]  
lhcData fn ls = do 
  CB.sourceFile fn
  =$= CB.lines
  =$= CM.skip 3
  =$= (CM.parserC LHCP.parsePosition)
  =$= (process ls)
  =$= (coll Nothing)
  =$= CM.debug
  $$ saveToFile-- CL.consume 
  
  
  

   
   {--
toFile :: CM.MonadResource m => Sink String m () 
toFile =    
  bracketP
  (SIO.openFile "test.txt" SIO.WriteMode)
  (\handle -> putStrLn "Closing handle" >> SIO.hClose handle)  
  write
  where write handle = awaitForever (\x -> CM.liftIO (SIO.hPutStrLn handle x))
        
        --}
        

  
ss :: CM.MonadResource m => Source m String
ss = do yield "1"
        yield "2"
        yield "3"
        yield "1"        
        
toFile2 :: CM.MonadResource m =>  Sink String m ()
toFile2 = do 
  xM <- await
  CM.liftIO $ print ("TF    " ++ (show xM))
  case xM of
    Nothing -> return ()
    Just x -> do CM.liftIO (E.bracket (SIO.openFile x SIO.AppendMode) (\handle -> putStrLn "Closing handle" >> SIO.hClose handle) (loop x))
                 toFile2
  
  
loop :: String -> SIO.Handle -> IO ()
loop x h = (SIO.hPutStrLn h x)
             
          
          
        
tt :: IO ()        
tt = CM.runResourceT (ss $$ toFile2)        
   
   
main = do
  ls <- EQP.earthQuakeList
  case ls of
    Left l -> print l
    Right r -> do k <- CM.runResourceT $ lhcData posData (reverse r)
                  print k
 
-- tR = (timeRange (60.0*60)) <$> LHCP.lhcD <*>  EQP.eqP     
 
 
--start :: IO (Either String [(EQP.EarthQuake, LHCP.POS_MEAN_H)])
--start = CM.runResourceT (sequence <$> lhcData posData)  