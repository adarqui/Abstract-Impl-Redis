module Abstract.Impl.Redis.Queue.Enq (
 module Abstract.Interfaces.Queue.Enq,
 mkQueue'Redis'Enq
) where

import Abstract.Interfaces.Queue.Enq

import qualified Abstract.Impl.Redis.Queue.Internal as REDIS (mkQueue'Redis)

mkQueue'Redis'Enq qrw = do
 v <- REDIS.mkQueue'Redis qrw
 return $ queueToEnq v
