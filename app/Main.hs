{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.ReverseProxy
import Network.Wai.Handler.Warp
import Network.Wai
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Control.Monad.Trans (liftIO)
import Data.CaseInsensitive (CI)
import Data.ByteString (ByteString)

import Debug.Trace

target = "www.example.com"
hostHeader = ("Host" :: CI ByteString, target)

addHostHeader :: Request -> Request
addHostHeader req =
  let oldHeaders = requestHeaders req
      oldHeadersWithouHost = filter (\(key,_) -> not (key == "Host")) oldHeaders
      newHeaders = oldHeadersWithouHost ++ [hostHeader]
   in traceShowId $ req {requestHeaders = newHeaders}

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  run 3001 $
    waiProxyToSettings
      (\ req -> return (WPRModifiedRequest
                         (addHostHeader req)
                         (ProxyDest target 80)))
      def
      manager
