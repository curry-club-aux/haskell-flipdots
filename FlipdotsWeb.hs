{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, OverloadedStrings #-}

module Main where

import Graphics.Flipdots
import Paths_flipdots (getDataDir)

import qualified Yesod.Core as YC
import qualified Yesod.Static as YS
import Options.Applicative
import qualified Network.SocketIO as SocketIO
import qualified Network.EngineIO.Yesod as EIOYesod
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as BS
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Control.Exception (catch, IOException)

newtype FlipboardImg = FlipboardImg BS.ByteString

instance FromJSON FlipboardImg where
  parseJSON = fmap (FlipboardImg . BS.pack) . parseJSON

instance ToJSON FlipboardImg where
  toJSON (FlipboardImg img) = toJSON (BS.unpack img)

data FlipdotsWeb =
  FlipdotsWeb { flipboardConfig :: FlipboardConfig
              , flipboardState :: STM.TVar FlipboardImg
              , getStatic :: YS.Static
              , socketIoHandler :: YC.HandlerT FlipdotsWeb IO ()
              }

YC.mkYesod "FlipdotsWeb" [YC.parseRoutesNoCheck|
/ IndexR GET
/static/ StaticR YS.Static getStatic
/socket.io/ SocketIOR
|]

instance YC.Yesod FlipdotsWeb where
  -- do not redirect /socket.io/?bla=blub to /socket.io?bla=blub
  cleanPath _ ["socket.io",""] = Right ["socket.io"]
  cleanPath _ p = Right p

getIndexR :: Handler ()
getIndexR = do
  dataDir <- YC.liftIO getDataDir
  YC.sendFile "text/html" $ dataDir ++ "/index.html"

handleSocketIOR :: Handler ()
handleSocketIOR = YC.getYesod >>= socketIoHandler

socketApp :: StateT SocketIO.RoutingTable
                    (ReaderT SocketIO.Socket
                             (YC.HandlerT FlipdotsWeb IO)) ()
socketApp = do
  FlipdotsWeb config state _ _ <- YC.getYesod
  SocketIO.on "new image" $ \img@(FlipboardImg bs) -> do
    liftIO $ do
      putStrLn "##### New image #####"
      STM.atomically $ STM.writeTVar state img
      catch (sendImage' bs config) handle
    SocketIO.broadcast "new image" img
  currImg <- liftIO $ STM.atomically $ STM.readTVar state
  SocketIO.emit "flipboard" $ object
    [ "dims" .= flipboardDims config
    , "image" .= currImg
    ]
  where
    handle :: IOException -> IO ()
    handle e = putStrLn (show e)

main :: IO ()
main = do
  config <- execParser opts
  let initialImg = FlipboardImg $ renderImage (\_x _y -> False) (flipboardDims config)
  flipboardState <- STM.newTVarIO initialImg
  socketIOHandler <- SocketIO.initialize EIOYesod.yesodAPI socketApp
  static <- YS.static =<< getDataDir
  let app = FlipdotsWeb config flipboardState static socketIOHandler
  putStrLn "Application running at http://localhost:8000/\n"
  YC.warp 8000 app
  where
    positiveInt = do
      a <- auto
      if a > 0
        then return a
        else fail "expected a positive integer"
    dimsP :: Parser Dims
    dimsP =
      Dims <$> option positiveInt (  long "rows" <> short 'r'
                                  <> value 16 <> showDefault
                                  <> metavar "ROWS" <> help "number of rows")
           <*> option positiveInt (   long "cols" <> short 'c'
                                  <> value 80 <> showDefault
                                  <> metavar "COLS" <> help "number of columns")
    flipboardConfigP :: Parser FlipboardConfig
    flipboardConfigP =
      FlipboardConfig
        <$> strOption (  long "host" <> short 'h'
                      <> value "flipdot.openlab.lan" <> showDefault
                      <> metavar "HOSTNAME" <> help "the network address of the flipboard")
        <*> option positiveInt (  long "port" <> short 'p'
                               <> value 2323 <> showDefault
                               <> metavar "PORTNUM" <> help "port number")
        <*> dimsP
    opts = info (helper <*> flipboardConfigP)
                (fullDesc <> progDesc "Web interface for drawing on a flipboard" <> header "flipdots webinterface")
