module Common (skip
               , R.MonadResource
               , R.runResourceT
               , T.UTCTime
               , T.NominalDiffTime
               , T.diffUTCTime
               , parseUTC
               , P.Parser
               , PC8.char
               , PC8.many1
               , PC8.notChar
               , PC8.parseOnly
               , parseWord
               , debug
               , parserC
               , cutOffC
               , comma 
               , MonadIO
               , MonadThrow
               , liftIO
               , Perhaps
               ) where 

import qualified Control.Monad.Trans.Resource  as R
import qualified Data.Time as T
import qualified Data.Attoparsec.ByteString as P
import qualified Data.Attoparsec.ByteString.Char8 as PC8
import Control.Monad.Trans(liftIO, MonadIO)
import Control.Monad.Catch(MonadThrow)
import qualified Data.ByteString as BS
import Data.Conduit

         
type Perhaps a = Either String a         
         
               
           {--
skip :: MonadResource m => Int -> Conduit a m a
skip n = go n
  where
    go x | x <=0 = (go 0)
    go x = await 
           >>= maybe (return ()) (process x) 
    
    process x a = 
       do 
          yield a
          go 0
          --}       
 

  
parseUTC :: String -> PC8.Parser String -> PC8.Parser T.UTCTime
parseUTC format parser = do 
  tm <- parser
  let mT = (T.parseTimeM False T.defaultTimeLocale format tm) -- :: Maybe T.UTCTime
  case mT of
    Nothing -> fail $ "Can't parse time" ++ tm
    Just x -> return x
 
 --maybe (fail ("Can't parse time" ++ tm)) (return) mT
  
   
  
 
debug :: 
  (Show a, MonadThrow m, MonadIO m)
  => Conduit a m a    
debug = 
  awaitForever $ \x -> (liftIO $ print x)>> (liftIO $ print "*****************************************************************************")
                    >> yield x  
 
 
 
comma :: P.Parser Char 
comma = PC8.char ',' 



parseWord:: P.Parser String
parseWord = PC8.many' $ noneOf [',']


                 
skip :: R.MonadResource m => Int -> Conduit a m a
skip n = go n
  where 
    go x = await 
           >>= maybe (return ()) (process x) 
    
    process x a = 
      if x == 0 
          then do 
            yield a
            go 0
          else 
            go (x-1)
            
  
  
noneOf :: [Char] -> P.Parser Char  
noneOf cs = PC8.satisfy (\c -> not (elem c cs)) 
  
  
  
parserC :: 
  MonadThrow m
  => P.Parser a -> Conduit BS.ByteString m (Either String a)  
parserC parser = 
  awaitForever(\x -> yield(PC8.parseOnly parser x))   
 
 
 
cutOffC ::    
  MonadThrow m     
  => Conduit (Either String a) m (Either String a) 
cutOffC = 
  await >>= maybe (return()) (process)
  where
    process x =
      case x of
        l@(Left _)  -> yield l
        r@(Right x) -> yield r >> cutOffC  
                 
