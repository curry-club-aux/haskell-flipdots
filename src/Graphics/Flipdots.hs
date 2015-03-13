module Flipboard
  ( Dims (..), FlipboardConfig (..), Img
  , renderImage, sendImage', sendImage
  ) where

import Network.Socket hiding (sendTo)
import Network.Socket.ByteString (sendTo)
import Data.Word8 (Word8)
import Data.ByteString as BS

data Dims =
  Dims
  { width :: Int
  , height :: Int
  } deriving (Eq, Show, Read)

data FlipboardConfig =
  FlipboardConfig
  { udpHostname :: String
  , udpPort :: Int
  , dims :: Dims
  } deriving (Eq, Show, Read)

type Img = Int -> Int -> Bool

renderImage :: Img -> Dims -> ByteString
renderImage img (Dims w h) = BS.pack $ toStr [ img x y | y <- [0..(h-1)], x <- [0..(w-1)] ]
  where
    i True x = x
    i False _ = 0
    toStr :: [Bool] -> [Word8]
    toStr (a:b:c:d:e:f:g:h:xs) =
      let num = i a 128 + i b 64 + i c 32 + i d 16 + i e 8 + i f 4 + i g 2 + i h 1
      --let num = i a 1 + i b 2 + i c 4 + i d 8 + i e 16 + i f 32 + i g 64 + i h 128
      in num : toStr xs
    toStr _ = []

sendImage' :: ByteString -> FlipboardConfig -> IO ()
sendImage' img (FlipboardConfig hostname port _dims) = do
  addrInfos <- getAddrInfo Nothing (Just hostname) (Just (show port))
  serverAddr <- case addrInfos of
    [] -> error "host not found"
    (i:_) -> return i
  sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
  let sendStr msg =
        if BS.null msg
          then return ()
          else do
            sent <- sendTo sock msg (addrAddress serverAddr)
            sendStr (BS.drop sent msg)
  print img
  sendStr img

sendImage :: Img -> FlipboardConfig -> IO ()
sendImage img cfg@(FlipboardConfig _hostname _port dims) =
  sendImage' (renderImage img dims) cfg
