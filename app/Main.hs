{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick.BChan
import qualified Client
import Config (Config)
import qualified Config
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as C8
import qualified Data.List.NonEmpty as NE
import Data.Strict.Classes (toLazy)
import Data.Text.Encoding (encodeUtf8)
import Network.Socket
import Network.Socket.ByteString
import Network.TLS
import System.Environment
import UserInterface.Main
import UserInterface.Types (RecvChan, SendChan, UiConfig (..))

-- | Create a TCP socket and connect it to the given hostname and port
connectTCP :: String -> String -> IO Socket
connectTCP hostname port = do
  let hints = defaultHints {addrSocketType = Stream}
  addr <- NE.head <$> getAddrInfo (Just hints) (Just hostname) (Just port)
  sock <- openSocket addr
  connect sock $ addrAddress addr
  return sock

-- | Run the client over TLS using the given configuration
runTLSClient :: Config -> RecvChan -> SendChan -> IO ()
runTLSClient config recvChan sendChan = do
  writeBChan recvChan "Connecting to the TCP server..."
  bracket (connectTCP hostname port) close $ \backend -> do
    writeBChan recvChan "Connected to the TCP server!"
    let hooks =
          if not checkCertificates
            then
              defaultClientHooks {onServerCertificate = \_ _ _ _ -> return []}
            else
              -- TODO: Properly handle certificate verification
              defaultClientHooks
    let params = (defaultParamsClient hostname (C8.pack port)) {clientHooks = hooks}
    writeBChan recvChan "Connecting to the TLS server..."
    bracket (contextNew backend params) bye $ \ctx -> do
      handshake ctx
      writeBChan recvChan "Connected to the TLS server!"
      let client = Client.Client (sendData ctx . toLazy . encodeUtf8) (recvData ctx) config recvChan sendChan
      Client.runClient client
  where
    serverConfig = Config.server config
    hostname = Config.hostname serverConfig
    port = show $ Config.port serverConfig
    checkCertificates = Config.checkCertificates serverConfig

-- | Run the client over TCP using the given configuration
runTCPClient :: Config -> RecvChan -> SendChan -> IO ()
runTCPClient config recvChan sendChan = do
  writeBChan recvChan "Connecting to the TCP server..."
  bracket (connectTCP hostname port) close $ \sock -> do
    writeBChan recvChan "Connected to the TCP server!"
    let client = Client.Client (void . send sock . encodeUtf8) (recv sock 2048) config recvChan sendChan
    Client.runClient client
  where
    serverConfig = Config.server config
    hostname = Config.hostname serverConfig
    port = show $ Config.port serverConfig

createUiConfig :: Config -> SendChan -> UiConfig
createUiConfig config chan =
  UiConfig
    { sendChannel = chan,
      sentHistoryLen = Config.sentHistoryLen clientConfig,
      channelHistoryLen = Config.channelHistoryLen clientConfig,
      initialChannels = "- Raw" : Config.channels serverConfig,
      displayName = Config.username userConfig
    }
  where
    clientConfig = Config.client config
    serverConfig = Config.server config
    userConfig = Config.user config

main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  case args of
    [filename] -> do
      config <- Config.parseConfig filename
      print config
      recvChan <- newBChan 2048
      sendChan <- newBChan 32
      let runClient = if Config.useTls . Config.server $ config then runTLSClient else runTCPClient
      let uiConfig = createUiConfig config sendChan
      race_ (runUserInterface uiConfig recvChan) (runClient config recvChan sendChan)
    _ -> putStrLn ("Usage: ./" ++ progName ++ " [configuration file]")
