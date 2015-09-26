import Data.Conduit
import qualified Data.Conduit.List as C
import qualified Data.Foldable as F
import qualified Data.Conduit.Binary as CB
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Monad.Trans.Resource

-- | Filter only pairs tagged with the appropriate key.
filterInputC :: (Monad m, Eq k) => k -> Conduit (k, a) m a
filterInputC idx = C.filter ((idx ==) . fst) =$= C.map snd

-- | Prepend a given sink with a filter.
filterInput :: (Monad m, Eq k) => k -> Sink a m r -> Sink (k, a) m r
filterInput idx = (filterInputC idx =$)

-- | Given a list of sinks, create a single sink that directs received values
-- depending on the index.
multiSink_ :: (Monad m) => [Sink a m ()] -> Sink (Int, a) m ()
multiSink_ = getZipSink . F.sequenceA_ . fmap ZipSink
             . zipWith filterInput [0..]
             
             
             
             
-- | A testing sink that just prints its input, marking it with
-- a given prefix.
testSink :: String -> Sink String IO ()
testSink prefix = C.mapM_ (putStrLn . (prefix ++))

convert :: MonadResource m  => Conduit (Int, String) m (Int, B.ByteString)
convert = awaitForever (\(i, x) -> yield (i, (BC.pack x)))

sinkF ::MonadResource m => FilePath -> Sink B.ByteString m ()
sinkF s = CB.sinkFile s

-- | An example that produces indexed output.
testSource :: (Monad m) => Source m (Int, String)
testSource = do
    yield (0, "abc")
    yield (0, "def")
    yield (1, "opq")
    yield (0, "0")
    yield (1, "1")
    yield (2, "rest")

--main :: IO ()
tst :: MonadResource m =>  m ()  
tst  = testSource 
       =$= convert
       $$ multiSink_ (map sinkF ["1: ", "2: ", "3: "])           
       
       
       
main = runResourceT tst      