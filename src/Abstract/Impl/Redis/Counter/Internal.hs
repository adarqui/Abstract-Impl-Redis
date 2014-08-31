{-# LANGUAGE OverloadedStrings #-}

module Abstract.Impl.Redis.Counter.Internal (
 module Abstract.Interfaces.Counter,
 CounterRedis,
 counterRedis'Int,
 defaultCounterRedis'Int,
 mkCounter'Redis'Int
) where

import Control.Exception

import Abstract.Interfaces.Counter

import qualified Abstract.Utilities.Redis.Helpers as H

import qualified Database.Redis as R
import qualified Data.ByteString.Char8 as B


data CounterRedis t = CounterRedis {
 _info :: R.ConnectInfo,
 _conn :: H.ConnectionWrapper,
 _key :: B.ByteString,
 _n :: t 
}


mkCounter'Redis'Int :: Int -> CounterRedis Int -> IO (Counter IO Int)
mkCounter'Redis'Int n crw = do
 let crw' = crw { _n = n }
 conn <- H.open $ _info crw'
 return $ defaultCounter'Redis'Int $ crw' { _conn = conn }


incr'Int :: CounterRedis Int -> IO Int
incr'Int w = do
 v <- H.incr (_conn w) (_key w)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right v') -> v'
  

incrBy'Int :: CounterRedis Int -> Int -> IO Int
incrBy'Int w n = do
 v <- H.incrBy (_conn w) (_key w) n 
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right v') -> v'


decr'Int :: CounterRedis Int -> IO Int
decr'Int w = do
 v <- H.decr (_conn w) (_key w)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right v') -> v'


decrBy'Int :: CounterRedis Int -> Int -> IO Int
decrBy'Int w n = do
 v <- H.decrBy (_conn w) (_key w) n 
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right v') -> v'


get' :: CounterRedis Int -> IO (Maybe Int)
get' w = do
 v <- H.getInt (_conn w) (_key w)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right Nothing) -> Nothing
  (Right (Just v')) -> Just v'


reset' :: CounterRedis Int -> IO ()
reset' w = do
 v <- H.set (_conn w) (_key w) (B.pack $ show $ _n w)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right _) -> ()


gentleReset' :: CounterRedis Int -> IO ()
gentleReset' w = do
 v <- H.setnx (_conn w) (_key w) (B.pack $ show $ _n w)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right _) -> ()


defaultCounterRedis'Int :: String -> CounterRedis Int
defaultCounterRedis'Int cname = counterRedis'Int cname R.defaultConnectInfo


counterRedis'Int :: String -> R.ConnectInfo -> CounterRedis Int
counterRedis'Int cname ci = CounterRedis { _info=ci, _key=B.pack cname }


defaultCounter'Redis'Int :: CounterRedis Int -> Counter IO Int
defaultCounter'Redis'Int w = do
 Counter {
  _incr = incr'Int w,
  _incrBy = incrBy'Int w,
  _decr = decr'Int w,
  _decrBy = decrBy'Int w,
  _get = get' w,
  _reset = reset' w,
  _gentleReset = gentleReset' w
 }
