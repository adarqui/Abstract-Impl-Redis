module Abstract.Impl.Redis.Counter (
 Counter (..),
 counterRedisWrapper'Int,
 defaultCounterRedisWrapper'Int,
 mkCounter'Redis'Int,
 mkCounter'Redis'Int'Inc,
 mkCounter'Redis'Int'Dec,
 mkCounter'Redis'Int'Get
) where

import Abstract.Interfaces.Counter
import Abstract.Impl.Redis.Counter.Internal (mkCounter'Redis'Int, defaultCounterRedisWrapper'Int, counterRedisWrapper'Int)
import Abstract.Impl.Redis.Counter.Inc (mkCounter'Redis'Int'Inc)
import Abstract.Impl.Redis.Counter.Dec (mkCounter'Redis'Int'Dec)
import Abstract.Impl.Redis.Counter.Get (mkCounter'Redis'Int'Get)
