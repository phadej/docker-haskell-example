{-# LANGUAGE DataKinds #-}
module Main (main) where

import Servant
import System.IO (stdout, hFlush)
import qualified Network.Wai.Handler.Warp as Warp

type ExampleAPI = Get '[JSON] [String]

exampleAPI :: Proxy ExampleAPI
exampleAPI = Proxy

exampleServer :: Server ExampleAPI
exampleServer = return ["hello", "world"]

main :: IO ()
main = do
    putStrLn "http://localhost:8000"
    hFlush stdout
    Warp.run 8000 $ serve exampleAPI exampleServer
