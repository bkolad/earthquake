{-# LANGUAGE RankNTypes #-}

module AppendAll where

import qualified System.Directory as D
import qualified Data.List as L
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Common as CM 
import qualified System.IO as SIO
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import Data.Conduit




dataDir ="/home/blazej/Programowanie/EarthQake/ROW_DATA/"

--dataDir = "/home/blazej/Programowanie/EarthQake/Scripts/haskellscripts/TEST/"
output = dataDir ++ "All.txt" 
            
hidden = not . L.isPrefixOf "."
         
         
         
files :: (FilePath -> Bool) -> FilePath -> IO [FilePath]      
files excludeDir dir =  
    do filesL <- D.getDirectoryContents dir
       return $ map (\oneFile -> dir ++ "/" ++oneFile) (filter excludeDir filesL) 

       
       
appendAll :: IO ()       
appendAll = do fs <- files hidden dataDir
               let sorted = L.sort fs
               mapM_ runAppend sorted
             where
               runAppend :: FilePath -> IO ()       
               runAppend fn = CM.runResourceT (appendCSV fn)    

               

unline :: Monad m => Conduit T.Text m T.Text   
unline = awaitForever (\ln -> yield $ ln `T.append` (T.singleton '\n'))    -- TODO instead of T.append add second yield (T.singleton '\n') 
 
    
                   
appSink :: CM.MonadResource m => Sink BS.ByteString m ()         
appSink = CB.sinkIOHandle $ SIO.openFile output SIO.AppendMode       

                 
                 
appendCSV :: CM.MonadResource m => FilePath -> m ()       
appendCSV fn = 
  CB.sourceFile fn
  =$= CT.decodeUtf8
  =$= CT.lines
  =$= CM.skip 3 --skip 2
  =$= unline
  =$= CT.encodeUtf8
  $$ appSink
        

  
    
