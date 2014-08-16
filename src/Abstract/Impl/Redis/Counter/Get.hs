module Abstract.Impl.Redis.Counter.Get (
 module Abstract.Interfaces.Counter.Get,
 mkCounter'Redis'Int'Get
) where

import Abstract.Interfaces.Counter.Get

import qualified Abstract.Impl.Redis.Counter.Internal as REDIS (mkCounter'Redis'Int)

mkCounter'Redis'Int'Get s t = do
 v <- REDIS.mkCounter'Redis'Int s t
 return $ counterToGet v
