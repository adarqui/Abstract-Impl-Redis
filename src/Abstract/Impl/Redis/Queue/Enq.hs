module Abstract.Impl.Redis.Queue.Enq (
 module Abstract.Interfaces.Queue.Enq,
 mkQueue'Redis'Enq
) where

import qualified Abstract.Interfaces.Queue as Q
import Abstract.Interfaces.Queue.Enq

import qualified Abstract.Impl.Redis.Queue.Internal as REDIS (mkQueue'Redis)

mkQueue'Redis'Enq s t pack unpack = do
 v <- REDIS.mkQueue'Redis s t pack unpack
 return $ queueToEnq v
