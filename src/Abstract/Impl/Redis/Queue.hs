module Abstract.Impl.Redis.Queue (
 Queue (..),
 queueRedis,
 defaultQueueRedis,
 mkQueue'Redis,
 mkQueue'Redis'Enq,
 mkQueue'Redis'Deq
) where

import Abstract.Interfaces.Queue
import Abstract.Impl.Redis.Queue.Internal (mkQueue'Redis, defaultQueueRedis, queueRedis)
import Abstract.Impl.Redis.Queue.Enq (mkQueue'Redis'Enq)
import Abstract.Impl.Redis.Queue.Deq (mkQueue'Redis'Deq)
