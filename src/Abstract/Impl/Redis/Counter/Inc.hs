module Abstract.Impl.Redis.Counter.Inc (
 module Abstract.Interfaces.Counter.Inc,
 mkCounter'Redis'Int'Inc
) where

import Abstract.Interfaces.Counter.Inc

import qualified Abstract.Impl.Redis.Counter.Internal as REDIS (mkCounter'Redis'Int)

mkCounter'Redis'Int'Inc s t = do
 v <- REDIS.mkCounter'Redis'Int s t
 return $ counterToInc v
