module Abstract.Impl.Redis.Stack (
 Stack (..),
 stackRedis,
 defaultStackRedis,
 mkStack'Redis,
 mkStack'Redis'Push,
 mkStack'Redis'Pop
) where

import Abstract.Interfaces.Stack
import Abstract.Impl.Redis.Stack.Internal (mkStack'Redis, stackRedis, defaultStackRedis)
import Abstract.Impl.Redis.Stack.Push (mkStack'Redis'Push)
import Abstract.Impl.Redis.Stack.Pop (mkStack'Redis'Pop)
