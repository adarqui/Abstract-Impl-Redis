module Abstract.Impl.Redis.Stack.Pop (
 module Abstract.Interfaces.Stack.Pop,
 mkStack'Redis'Pop
) where

import qualified Abstract.Interfaces.Stack as S
import Abstract.Interfaces.Stack.Pop

import qualified Abstract.Impl.Redis.Stack.Internal as REDIS (mkStack'Redis)

mkStack'Redis'Pop s t pack unpack = do
 v <- REDIS.mkStack'Redis s t pack unpack
 return $ stackToPop v
