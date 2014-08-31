module Abstract.Impl.Redis.Stack.Push (
 module Abstract.Interfaces.Stack.Push,
 mkStack'Redis'Push
) where

import Abstract.Interfaces.Stack.Push

import qualified Abstract.Impl.Redis.Stack.Internal as REDIS (mkStack'Redis)

mkStack'Redis'Push srw = do
 v <- REDIS.mkStack'Redis srw
 return $ stackToPush v
