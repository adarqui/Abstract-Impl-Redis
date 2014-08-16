module Abstract.Impl.Redis.Counter.Dec (
 module Abstract.Interfaces.Counter.Dec,
 mkCounter'Redis'Int'Dec
) where

import Abstract.Interfaces.Counter.Dec

import qualified Abstract.Impl.Redis.Counter.Internal as REDIS (mkCounter'Redis'Int)

mkCounter'Redis'Int'Dec s t = do
 v <- REDIS.mkCounter'Redis'Int s t
 return $ counterToDec v
