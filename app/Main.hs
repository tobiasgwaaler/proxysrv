{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Main where

import Network.HTTP.ReverseProxy
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Control.Monad.Trans (liftIO)
import Data.CaseInsensitive (CI)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Options.Generic

hostHeader :: ByteString -> (CI ByteString, ByteString)
hostHeader dest = ("Host" :: CI ByteString, dest)

data CLIOptions = CLIOptions { destination :: String, port :: Int }
  deriving (Generic, Show)

instance ParseRecord CLIOptions

setHostHeaderToDestination :: ByteString -> Request -> Request
setHostHeaderToDestination dest req =
  req {requestHeaders = replaceHostHeader (requestHeaders req)}
  where replaceHostHeader =
          map (\ header@(name, _) ->
            case name of
              "Host" -> hostHeader dest
              _      -> header)

main :: IO ()
main = do
  opts <- getRecord "proxysrv"
  let dest = destination opts
      destBS = pack dest
  manager <- newManager defaultManagerSettings
  run (port opts) $ logStdout $
    waiProxyToSettings
      (\ req -> return (WPRModifiedRequest
                         (setHostHeaderToDestination destBS req)
                         (ProxyDest destBS 80)))
      def
      manager
