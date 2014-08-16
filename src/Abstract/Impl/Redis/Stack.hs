module Abstract.Impl.Redis.Stack (
 Stack (..),
 mkStack'Redis,
 mkStack'Redis'Push,
 mkStack'Redis'Pop
) where

import Abstract.Interfaces.Stack
import Abstract.Impl.Redis.Stack.Internal (mkStack'Redis)
import Abstract.Impl.Redis.Stack.Push (mkStack'Redis'Push)
import Abstract.Impl.Redis.Stack.Pop (mkStack'Redis'Pop)
