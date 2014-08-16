{-# LANGUAGE OverloadedStrings #-}

module Abstract.Impl.Redis.Queue.Internal (
 module Abstract.Interfaces.Queue,
 QueueRedisWrapper,
 defaultQueueRedisWrapper,
 mkQueue'Redis
) where

import Control.Exception

import Abstract.Interfaces.Queue
import qualified Abstract.Utilities.Redis.Helpers as H

import qualified Database.Redis as R
import qualified Data.ByteString.Char8 as B


data QueueRedisWrapper t = QueueRedisWrapper {
 _info :: R.ConnectInfo,
 _conn :: H.ConnectionWrapper,
 _key :: B.ByteString,
 _unpack :: B.ByteString -> t,
 _pack :: t -> B.ByteString
}


mkQueue'Redis :: String -> t -> ({-pack-}t->B.ByteString) -> ({-unpack-}B.ByteString->t) -> IO (Queue IO (QueueRedisWrapper t) t)
mkQueue'Redis qname t pack unpack = do
 let qrw = defaultQueueRedisWrapper qname t pack unpack
 conn <- H.open $ _info qrw
 return $ defaultQueueWrapper qname $ qrw { _conn = conn }
 

enqueue' :: QueueRedisWrapper t -> t -> IO ()
enqueue' w t = do
 v <- H.enqueue (_conn w) (_key w) [((_pack w) t)]
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right _) -> ()


enqueueBatch' :: QueueRedisWrapper t -> [t] -> IO ()
enqueueBatch' w ts = do
 v <- H.enqueueBatch (_conn w) (_key w) (map (_pack w) ts)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right _) -> ()


dequeue' :: QueueRedisWrapper t -> IO (Maybe t)
dequeue' w = do
 v <- H.dequeue (_conn w) (_key w)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right Nothing) -> Nothing
  (Right (Just v')) -> Just $ _unpack w $ v'


drain' :: QueueRedisWrapper t -> IO [t]
drain' w = do
 v <- H.dequeueBatch (_conn w) (_key w)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right v') -> map (_unpack w) v'


size' :: QueueRedisWrapper t -> IO Int
size' w = do
 v <- H.llen (_conn w) (_key w)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right v') -> v'


destroy' :: QueueRedisWrapper t -> IO ()
destroy' w = do
 v <- H.del (_conn w) (_key w)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right _) -> ()


defaultQueueRedisWrapper :: String -> t -> (t -> B.ByteString) -> (B.ByteString -> t) -> QueueRedisWrapper t
defaultQueueRedisWrapper qname _ pack unpack = QueueRedisWrapper { _info = R.defaultConnectInfo, _key = B.pack qname, _pack = pack, _unpack = unpack }


defaultQueueWrapper :: String -> QueueRedisWrapper t -> Queue IO (QueueRedisWrapper t) t
defaultQueueWrapper qname w =
 Queue {
  _q = w,
  _qname = qname,
  _enqueue = enqueue',
  _enqueueBatch = enqueueBatch',
  _dequeue = dequeue',
  _drain = drain',
  _size = size',
  _destroy = destroy'
 }
