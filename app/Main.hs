{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.ReverseProxy
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Control.Monad.Trans (liftIO)
import Data.CaseInsensitive (CI)
import Data.ByteString (ByteString)

destination = "www.example.com"
hostHeader = ("Host" :: CI ByteString, destination)

setHostHeaderToDestination :: Request -> Request
setHostHeaderToDestination req =
  req {requestHeaders = replaceHostHeader (requestHeaders req)}
  where replaceHostHeader =
          map (\ header@(name, _) ->
            case name of
              "Host" -> hostHeader
              _      -> header)

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  run 3001 $ logStdout $
    waiProxyToSettings
      (\ req -> return (WPRModifiedRequest
                         (setHostHeaderToDestination req)
                         (ProxyDest destination 80)))
      def
      manager
