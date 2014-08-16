module Abstract.Impl.Redis.Queue.Deq (
 module Abstract.Interfaces.Queue.Deq,
 mkQueue'Redis'Deq
) where

import qualified Abstract.Interfaces.Queue as Q
import Abstract.Interfaces.Queue.Deq

import qualified Abstract.Impl.Redis.Queue.Internal as REDIS (mkQueue'Redis)

mkQueue'Redis'Deq s t pack unpack = do
 v <- REDIS.mkQueue'Redis s t pack unpack
 return $ queueToDeq v
