{-# LANGUAGE OverloadedStrings #-}

module Abstract.Impl.Redis.Counter.Internal (
 module Abstract.Interfaces.Counter,
 CounterRedisWrapper,
 counterRedisWrapper'Int,
 defaultCounterRedisWrapper'Int,
 mkCounter'Redis'Int
) where

import Control.Exception

import Abstract.Interfaces.Counter

import qualified Abstract.Utilities.Redis.Helpers as H

import qualified Database.Redis as R
import qualified Data.ByteString.Char8 as B


data CounterRedisWrapper t = CounterRedisWrapper {
 _info :: R.ConnectInfo,
 _conn :: H.ConnectionWrapper,
 _key :: B.ByteString,
 _n :: t 
}


mkCounter'Redis'Int :: Int -> CounterRedisWrapper Int -> IO (Counter IO (CounterRedisWrapper Int) Int)
mkCounter'Redis'Int n crw = do
 let crw' = crw { _n = n }
 conn <- H.open $ _info crw'
 return $ defaultCounterWrapper'Int $ crw' { _conn = conn }


incr'Int :: CounterRedisWrapper Int -> IO Int
incr'Int w = do
 v <- H.incr (_conn w) (_key w)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right v') -> v'
  

incrBy'Int :: CounterRedisWrapper Int -> Int -> IO Int
incrBy'Int w n = do
 v <- H.incrBy (_conn w) (_key w) n 
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right v') -> v'


decr'Int :: CounterRedisWrapper Int -> IO Int
decr'Int w = do
 v <- H.decr (_conn w) (_key w)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right v') -> v'


decrBy'Int :: CounterRedisWrapper Int -> Int -> IO Int
decrBy'Int w n = do
 v <- H.decrBy (_conn w) (_key w) n 
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right v') -> v'


get' :: CounterRedisWrapper Int -> IO (Maybe Int)
get' w = do
 v <- H.getInt (_conn w) (_key w)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right Nothing) -> Nothing
  (Right (Just v')) -> Just v'


reset' :: CounterRedisWrapper Int -> IO ()
reset' w = do
 v <- H.set (_conn w) (_key w) (B.pack $ show $ _n w)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right _) -> ()


gentleReset' :: CounterRedisWrapper Int -> IO ()
gentleReset' w = do
 v <- H.setnx (_conn w) (_key w) (B.pack $ show $ _n w)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right _) -> ()


defaultCounterRedisWrapper'Int :: String -> CounterRedisWrapper Int
defaultCounterRedisWrapper'Int cname = counterRedisWrapper'Int cname R.defaultConnectInfo


counterRedisWrapper'Int :: String -> R.ConnectInfo -> CounterRedisWrapper Int
counterRedisWrapper'Int cname ci = CounterRedisWrapper { _info=ci, _key=B.pack cname }


defaultCounterWrapper'Int :: CounterRedisWrapper Int -> Counter IO (CounterRedisWrapper Int) Int
defaultCounterWrapper'Int w = do
 Counter {
  _c = w,
  _incr = incr'Int,
  _incrBy = incrBy'Int,
  _decr = decr'Int,
  _decrBy = decrBy'Int,
  _get = get',
  _reset = reset',
  _gentleReset = gentleReset'
 }
