module Abstract.Impl.Redis.Stack.Push (
 module Abstract.Interfaces.Stack.Push,
 mkStack'Redis'Push
) where

import qualified Abstract.Interfaces.Stack as S
import Abstract.Interfaces.Stack.Push

import qualified Abstract.Impl.Redis.Stack.Internal as REDIS (mkStack'Redis)

mkStack'Redis'Push s t pack unpack = do
 v <- REDIS.mkStack'Redis s t pack unpack
 return $ stackToPush v
