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
  let oldHeaders = requestHeaders req
      oldHeadersWithouHost = filter (\(key,_) -> key /= "Host") oldHeaders
      newHeaders = oldHeadersWithouHost ++ [hostHeader]
   in req {requestHeaders = newHeaders}

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
