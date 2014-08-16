{-# LANGUAGE OverloadedStrings #-}

module Abstract.Impl.Redis.Stack.Internal (
 module Abstract.Interfaces.Stack,
 StackRedisWrapper,
 defaultStackRedisWrapper,
 mkStack'Redis
) where

import Control.Exception

import Abstract.Interfaces.Stack

import qualified Abstract.Utilities.Redis.Helpers as H

import qualified Database.Redis as R
import qualified Data.ByteString.Char8 as B


data StackRedisWrapper t = StackRedisWrapper {
 _info :: R.ConnectInfo,
 _conn :: H.ConnectionWrapper,
 _key :: B.ByteString,
 _unpack :: B.ByteString -> t,
 _pack :: t -> B.ByteString
}

mkStack'Redis :: String -> t -> ({-pack-}t->B.ByteString) -> ({-unpack-}B.ByteString->t) -> IO (Stack IO (StackRedisWrapper t) t)
mkStack'Redis sname t pack unpack = do
 let srw = defaultStackRedisWrapper sname t pack unpack
 conn <- H.open $ _info srw
 return $ defaultStackWrapper sname $ srw { _conn = conn }
 

push' :: StackRedisWrapper t -> t -> IO ()
push' w t = do
 v <- H.push (_conn w) (_key w) [((_pack w) t)]
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right _) -> ()


pushBatch' :: StackRedisWrapper t -> [t] -> IO ()
pushBatch' w ts = do
 v <- H.pushBatch (_conn w) (_key w) (map (_pack w) ts)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right _) -> ()


pop' :: StackRedisWrapper t -> IO (Maybe t)
pop' w = do
 v <- H.pop (_conn w) (_key w)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right Nothing) -> Nothing
  (Right (Just v')) -> Just $ _unpack w $ v'


drain' :: StackRedisWrapper t -> IO [t]
drain' w = do
 v <- H.popBatch (_conn w) (_key w)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right v') -> map (_unpack w) v'


size' :: StackRedisWrapper t -> IO Int
size' w = do
 v <- H.llen (_conn w) (_key w)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right v') -> v'


destroy' :: StackRedisWrapper t -> IO ()
destroy' w = do
 v <- H.del (_conn w) (_key w)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right _) -> ()


defaultStackRedisWrapper :: String -> t -> (t -> B.ByteString) -> (B.ByteString -> t) -> StackRedisWrapper t
defaultStackRedisWrapper sname _ pack unpack = StackRedisWrapper { _info = R.defaultConnectInfo, _key = B.pack sname, _pack = pack, _unpack = unpack }

defaultStackWrapper :: String -> StackRedisWrapper t -> Stack IO (StackRedisWrapper t) t
defaultStackWrapper sname w =
 Stack {
  _s = w,
  _sname = sname,
  _push = push',
  _pushBatch = pushBatch',
  _pop = pop',
  _drain = drain',
  _size = size',
  _destroy = destroy'
 }
 
