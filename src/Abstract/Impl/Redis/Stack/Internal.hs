{-# LANGUAGE OverloadedStrings #-}

module Abstract.Impl.Redis.Stack.Internal (
 module Abstract.Interfaces.Stack,
 StackRedis,
 defaultStackRedis,
 mkStack'Redis
) where

import Control.Exception

import Abstract.Interfaces.Stack

import qualified Abstract.Utilities.Redis.Helpers as H

import qualified Database.Redis as R
import qualified Data.ByteString.Char8 as B


data StackRedis t = StackRedis {
 _info :: R.ConnectInfo,
 _conn :: H.ConnectionWrapper,
 _key :: B.ByteString,
 _unpack :: B.ByteString -> t,
 _pack :: t -> B.ByteString
}

mkStack'Redis :: StackRedis t -> IO (Stack IO t)
mkStack'Redis srw = do
 conn <- H.open $ _info srw
 return $ buildStack $ srw { _conn = conn }
 

push' :: StackRedis t -> t -> IO ()
push' w t = do
 v <- H.push (_conn w) (_key w) [((_pack w) t)]
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right _) -> ()


pushBatch' :: StackRedis t -> [t] -> IO ()
pushBatch' w ts = do
 v <- H.pushBatch (_conn w) (_key w) (map (_pack w) ts)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right _) -> ()


pop' :: StackRedis t -> IO (Maybe t)
pop' w = do
 v <- H.pop (_conn w) (_key w)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right Nothing) -> Nothing
  (Right (Just v')) -> Just $ _unpack w $ v'


drain' :: StackRedis t -> IO [t]
drain' w = do
 v <- H.popBatch (_conn w) (_key w)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right v') -> map (_unpack w) v'


size' :: StackRedis t -> IO Int
size' w = do
 v <- H.llen (_conn w) (_key w)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right v') -> v'


destroy' :: StackRedis t -> IO ()
destroy' w = do
 v <- H.del (_conn w) (_key w)
 return $ case v of
  (Left _) -> throw OperationFailed
  (Right _) -> ()


defaultStackRedis :: String -> t -> (t -> B.ByteString) -> (B.ByteString -> t) -> StackRedis t
defaultStackRedis sname t pack unpack = stackRedis sname t pack unpack R.defaultConnectInfo


stackRedis :: String -> t -> (t -> B.ByteString) -> (B.ByteString -> t) -> R.ConnectInfo -> StackRedis t
stackRedis sname _ pack unpack ci = StackRedis { _info = ci, _key = B.pack sname, _pack = pack, _unpack = unpack }


buildStack :: StackRedis t -> Stack IO t
buildStack w =
 Stack {
  _push = push' w,
  _pushBatch = pushBatch' w,
  _pop = pop' w,
  _drain = drain' w,
  _size = size' w,
  _destroy = destroy' w
 } 
