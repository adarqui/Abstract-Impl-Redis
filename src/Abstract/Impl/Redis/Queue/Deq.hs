module Abstract.Impl.Redis.Queue.Deq (
 module Abstract.Interfaces.Queue.Deq,
 mkQueue'Redis'Deq
) where

import Abstract.Interfaces.Queue.Deq

import qualified Abstract.Impl.Redis.Queue.Internal as REDIS (mkQueue'Redis)

mkQueue'Redis'Deq qrw  = do
 v <- REDIS.mkQueue'Redis qrw
 return $ queueToDeq v
