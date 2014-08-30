module Abstract.Impl.Redis.Counter.Inc (
 module Abstract.Interfaces.Counter.Inc,
 mkCounter'Redis'Int'Inc
) where

import Abstract.Interfaces.Counter.Inc

import qualified Abstract.Impl.Redis.Counter.Internal as REDIS (mkCounter'Redis'Int)

mkCounter'Redis'Int'Inc t w = do
 v <- REDIS.mkCounter'Redis'Int t w
 return $ counterToInc v
