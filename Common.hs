module Common (skip
               , MonadResource
               , runResourceT
               ) where 

import Data.Conduit
import Control.Monad.Trans.Resource 
         
         
         
         
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
            
            
                 
skip :: MonadResource m => Int -> Conduit a m a
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
            
                 
