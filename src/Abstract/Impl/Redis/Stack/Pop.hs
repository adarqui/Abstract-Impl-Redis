module Abstract.Impl.Redis.Stack.Pop (
 module Abstract.Interfaces.Stack.Pop,
 mkStack'Redis'Pop
) where

import Abstract.Interfaces.Stack.Pop

import qualified Abstract.Impl.Redis.Stack.Internal as REDIS (mkStack'Redis)

mkStack'Redis'Pop srw = do
 v <- REDIS.mkStack'Redis srw
 return $ stackToPop v
