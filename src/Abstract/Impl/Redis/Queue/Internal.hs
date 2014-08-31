{-# LANGUAGE OverloadedStrings #-}

module Abstract.Impl.Redis.Queue.Internal (
 module Abstract.Interfaces.Queue,
 QueueRedis,
 queueRedis,
 defaultQueueRedis,
 mkQueue'Redis
) where

import Control.Exception

import Abstract.Interfaces.Queue
import qualified Abstract.Utilities.Redis.Helpers as H

import qualified Database.Redis as R
import qualified Data.ByteString.Char8 as B


data QueueRedis t = QueueRedis {
 _info :: R.ConnectInfo,
 _conn :: H.ConnectionWrapper,
 _key :: B.ByteString,
 _unpack :: B.ByteString -> t,
 _pack :: t -> B.ByteString
}


mkQueue'Redis :: QueueRedis t -> IO (Queue IO t)
mkQueue'Redis qrw = do
 conn <- H.open $ _info qrw
 return $ buildQueue $ qrw { _conn = conn }
 

enqueue' :: QueueRedis t -> t -> IO ()
enqueue' w t = do
 v <- H.enqueue (_conn w) (_key w) [((_pack w) t)]
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right _) -> ()


enqueueBatch' :: QueueRedis t -> [t] -> IO ()
enqueueBatch' w ts = do
 v <- H.enqueueBatch (_conn w) (_key w) (map (_pack w) ts)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right _) -> ()


dequeue' :: QueueRedis t -> IO (Maybe t)
dequeue' w = do
 v <- H.dequeue (_conn w) (_key w)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right Nothing) -> Nothing
  (Right (Just v')) -> Just $ _unpack w $ v'


drain' :: QueueRedis t -> IO [t]
drain' w = do
 v <- H.dequeueBatch (_conn w) (_key w)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right v') -> map (_unpack w) v'


size' :: QueueRedis t -> IO Int
size' w = do
 v <- H.llen (_conn w) (_key w)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right v') -> v'


destroy' :: QueueRedis t -> IO ()
destroy' w = do
 v <- H.del (_conn w) (_key w)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right _) -> ()


defaultQueueRedis :: String -> t -> (t -> B.ByteString) -> (B.ByteString -> t) -> QueueRedis t
defaultQueueRedis qname t pack unpack = queueRedis qname t pack unpack R.defaultConnectInfo


queueRedis :: String -> t -> (t -> B.ByteString) -> (B.ByteString -> t) -> R.ConnectInfo -> QueueRedis t
queueRedis qname _ pack unpack ci = QueueRedis { _info = ci, _key = B.pack qname, _pack = pack, _unpack = unpack }


buildQueue :: QueueRedis t -> Queue IO t
buildQueue w =
 Queue {
  _enqueue = enqueue' w,
  _enqueueBatch = enqueueBatch' w,
  _dequeue = dequeue' w,
  _drain = drain' w,
  _size = size' w,
  _destroy = destroy' w
 }
