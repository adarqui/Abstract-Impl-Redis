module Abstract.Impl.Redis.Counter (
 Counter (..),
 counterRedis'Int,
 defaultCounterRedis'Int,
 mkCounter'Redis'Int,
 mkCounter'Redis'Int'Inc,
 mkCounter'Redis'Int'Dec,
 mkCounter'Redis'Int'Get
) where

import Abstract.Interfaces.Counter
import Abstract.Impl.Redis.Counter.Internal (mkCounter'Redis'Int, defaultCounterRedis'Int, counterRedis'Int)
import Abstract.Impl.Redis.Counter.Inc (mkCounter'Redis'Int'Inc)
import Abstract.Impl.Redis.Counter.Dec (mkCounter'Redis'Int'Dec)
import Abstract.Impl.Redis.Counter.Get (mkCounter'Redis'Int'Get)
