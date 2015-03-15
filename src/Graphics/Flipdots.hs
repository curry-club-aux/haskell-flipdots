{-# LANGUAGE OverloadedStrings #-}

module Graphics.Flipdots
  ( Dims (..), FlipboardConfig (..), Img
  , renderImage, sendImage', sendImage
  ) where

import Network.Socket hiding (sendTo)
import Network.Socket.ByteString (sendTo)
import Data.Word8 (Word8)
import Data.ByteString as BS
import Data.Aeson
import Control.Applicative ((<$>), (<*>))

data Dims =
  Dims
  { dimsRows :: Int
  , dimsCols :: Int
  } deriving (Eq, Show, Read)

instance ToJSON Dims where
  toJSON (Dims r c) = object [ "rows" .= r, "cols" .= c ]

instance FromJSON Dims where
  parseJSON (Object o) = Dims <$> o .: "rows" <*> o .: "cols"
  parseJSON _ = fail "expected an object"

data FlipboardConfig =
  FlipboardConfig
  { flipboardHostname :: String
  , flipboardPort :: Int
  , flipboardDims :: Dims
  } deriving (Eq, Show, Read)

type Img = Int -> Int -> Bool

renderImage :: Img -> Dims -> ByteString
renderImage img (Dims rows cols) =
  BS.pack $ toWordList [ img x y | y <- [0..(rows-1)], x <- [0..(cols-1)] ]
  where
    i True x = x
    i False _ = 0
    toWordList :: [Bool] -> [Word8]
    toWordList (a:b:c:d:e:f:g:h:xs) =
      let word = i a 128 + i b 64 + i c 32 + i d 16 + i e 8 + i f 4 + i g 2 + i h 1
      in word : toWordList xs
    toWordList _ = []

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
