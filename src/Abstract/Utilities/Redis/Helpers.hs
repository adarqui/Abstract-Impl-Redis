{-# LANGUAGE OverloadedStrings #-}
module Abstract.Utilities.Redis.Helpers (
 ConnectionWrapper(..),
 open,
 close,
 get,
 set,
 setnx,
 llen,
 del,
 incr,
 decr,
 incrBy,
 decrBy,
 getInt,
 reset,
 gentleReset,
 enqueue,
 enqueueBatch,
 dequeue,
 dequeueBatch,
 blDequeue,
 push,
 pushBatch,
 pop,
 popBatch,
 blPop
) where


import Database.Redis hiding (get, del, set, setnx, llen, incr, decr, incrby, decrby, keys)
import qualified Database.Redis as R
import qualified Data.ByteString.Char8 as B


data ConnectionWrapper = ConnectionWrapper {
 _conn :: Connection
}


open :: ConnectInfo -> IO ConnectionWrapper
open ci = do
 conn <- connect $ ci
 return $ ConnectionWrapper { _conn = conn }


close :: ConnectionWrapper -> IO ()
close w = do
 _ <- runRedis (_conn w) $ quit
 return ()


get :: ConnectionWrapper -> B.ByteString -> IO (Either () (Maybe B.ByteString))
get w key = do
 v <- runRedis (_conn w) $ R.get key
 return $ case v of
  (Left _) -> Left ()
  (Right v') -> Right v'


set :: ConnectionWrapper -> B.ByteString -> B.ByteString -> IO (Either () ())
set w key val = do
 v <- runRedis (_conn w) $ R.set key val
 return $ case v of
  (Left _) -> Left ()
  (Right _) -> Right ()


setnx :: ConnectionWrapper -> B.ByteString -> B.ByteString -> IO (Either () Bool)
setnx w key val = do
 v <- runRedis (_conn w) $ R.setnx key val
 return $ case v of
  (Left _) -> Left ()
  (Right v') -> Right v'


llen :: ConnectionWrapper -> B.ByteString -> IO (Either () Int)
llen w key = do
 v <- runRedis (_conn w) $ R.llen key
 return $ case v of
  (Left _) -> Left ()
  (Right v') -> Right $ fromIntegral v'


del :: ConnectionWrapper -> B.ByteString -> IO (Either () Int)
del w key = do
 v <- runRedis (_conn w) $ R.del [key]
 return $ case v of
  (Left _) -> Left ()
  (Right v') -> Right $ fromIntegral v'


incr :: ConnectionWrapper -> B.ByteString -> IO (Either () Int)
incr w key = do
 v <- runRedis (_conn w) $ R.incr key
 return $ case v of
  (Left _) -> Left ()
  (Right n) -> Right $ fromIntegral n


decr ::ConnectionWrapper -> B.ByteString -> IO (Either () Int)
decr w key = do
 v <- runRedis (_conn w) $ R.decr key
 return $ case v of
  (Left _) -> Left ()
  (Right n) -> Right $ fromIntegral n


incrBy :: (ConnectionWrapper) -> B.ByteString -> Int -> IO (Either () Int)
incrBy w key by = do
 v <- runRedis (_conn w) $ R.incrby key (fromIntegral by)
 return $ case v of
  (Left _) -> Left ()
  (Right n) -> Right $ fromIntegral n


decrBy :: ConnectionWrapper -> B.ByteString -> Int -> IO (Either () Int)
decrBy w key by = do
 v <- runRedis (_conn w) $ R.decrby key (fromIntegral by)
 return $ case v of
  (Left _) -> Left ()
  (Right n) -> Right $ fromIntegral n


getInt :: ConnectionWrapper -> B.ByteString -> IO (Either () (Maybe Int))
getInt w key = do
 v <- runRedis (_conn w) $ R.get key
 return $ case v of
  (Left _) -> Left ()
  (Right Nothing) -> Right Nothing
  (Right (Just s)) -> Right $ Just $ (read (B.unpack s) :: Int)


reset :: ConnectionWrapper -> B.ByteString -> Int -> IO (Either () ())
reset w key n = do
 set w key (B.pack $ show n)


gentleReset :: ConnectionWrapper -> B.ByteString -> Int -> IO (Either () Bool)
gentleReset w key n = do
 setnx w key (B.pack $ show n)


enqueue :: ConnectionWrapper -> B.ByteString -> [B.ByteString] -> IO (Either () ())
enqueue w key xs = do
 v <- runRedis (_conn w) $ rpush key xs
 return $ case v of
  (Left _) -> Left ()
  (Right _) -> Right ()


enqueueBatch :: ConnectionWrapper -> B.ByteString -> [B.ByteString] -> IO (Either () ())
enqueueBatch w key xs = do
 v <- runRedis (_conn w) $ rpush key xs
 return $ case v of
  (Left _) -> Left ()
  (Right _) -> Right ()


dequeue :: ConnectionWrapper -> B.ByteString -> IO (Either () (Maybe B.ByteString))
dequeue w key = do
 v <- runRedis (_conn w) $ lpop key
 return $ case v of
  (Left _) -> Left ()
  (Right v') -> case v' of
   (Just v'') -> return $ Just v''
   _ -> return Nothing


dequeueBatch :: ConnectionWrapper -> B.ByteString -> IO (Either () [B.ByteString])
dequeueBatch w key = do
 dequeueBatch' w key []


dequeueBatch' :: ConnectionWrapper -> B.ByteString -> [B.ByteString] -> IO (Either () ([B.ByteString]))
dequeueBatch' w key accum = do
 v <- dequeue w key
 case v of
  (Left _) -> return $ Left ()
  (Right v') -> case v' of
   Nothing -> return $ Right accum
   (Just v'') -> do
    v''' <- dequeueBatch' w key (accum ++ [v''])
    return v'''


blDequeue :: ConnectionWrapper -> [B.ByteString] -> IO (Either () (Maybe (B.ByteString, B.ByteString)))
blDequeue w keys = do
 v <- runRedis (_conn w) $ blpop keys 0
 return $ case v of
  (Left _) -> Left ()
  (Right v') -> case v' of
   Nothing -> Left ()
   (Just v'') -> Right $ Just v''



push :: ConnectionWrapper -> B.ByteString -> [B.ByteString] -> IO (Either () ())
push w key xs = do
 v <- runRedis (_conn w) $ rpush key xs
 return $ case v of
  (Left _) -> Left ()
  (Right _) -> Right ()


pushBatch :: ConnectionWrapper -> B.ByteString -> [B.ByteString] -> IO (Either () ())
pushBatch w key xs = do
 v <- runRedis (_conn w) $ rpush key xs
 return $ case v of
  (Left _) -> Left ()
  (Right _) -> Right ()


pop :: ConnectionWrapper -> B.ByteString -> IO (Either () (Maybe B.ByteString))
pop w key = do
 v <- runRedis (_conn w) $ rpop key
 return $ case v of
  (Left _) -> Left ()
  (Right v') -> case v' of
   (Just v'') -> Right $ Just v''
   _ -> Right Nothing


popBatch :: ConnectionWrapper -> B.ByteString -> IO (Either () [B.ByteString])
popBatch w key = do
 popBatch' w key []


popBatch' :: ConnectionWrapper -> B.ByteString -> [B.ByteString] -> IO (Either () ([B.ByteString]))
popBatch' w key accum = do
 v <- pop w key
 case v of
  (Left _) -> return $ Left ()
  (Right v') -> case v' of
   Nothing -> return $ Right accum
   (Just v'') -> do
    v''' <- popBatch' w key (accum ++ [v''])
    return v'''


blPop :: ConnectionWrapper -> [B.ByteString] -> IO (Either () (Maybe (B.ByteString, B.ByteString)))
blPop w keys = do
 v <- runRedis (_conn w) $ brpop keys 0
 return $ case v of
  (Left _) -> Left ()
  (Right v') -> case v' of
   Nothing -> Right Nothing
   (Just v'') -> Right$ Just v''
