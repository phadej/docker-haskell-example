{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad (when)
import Servant
import System.IO (stdout, hFlush)
import qualified Network.Wai.Handler.Warp as Warp
import System.Posix.Env.ByteString (getEnv)
import Data.ByteString (ByteString)
import FileEmbedLzma (embedByteString)
import Data.ByteString.Base64 (decodeLenient)
import OpenSSL (withOpenSSL)
import OpenSSL.EVP.Digest (pkcs5_pbkdf2_hmac_sha1)
import OpenSSL.EVP.Cipher (getCipherByName, CryptoMode(Decrypt), cipherBS, getCipherNames)

import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

encrypted :: ByteString
encrypted = $(embedByteString "encrypted.txt")

encrypted' :: ByteString
encrypted' = decodeLenient encrypted

iters :: Int
iters = 100000

extract
    :: ByteString        -- ^ password
    -> ByteString        -- ^ encrypted data
    -> IO ByteString  -- ^ decrypted data
extract password bs0 = do
    when (BS.length bs0 < 16) $ fail "Too small input"

    let (magic, bs1) = BS.splitAt 8 bs0
        (salt,  enc) = BS.splitAt 8 bs1

    when (magic /= "Salted__") $ fail "No Salted__ header"
    -- BS8.putStrLn $ "salt=" <> Base16.encode salt

    let (key, iv) = BS.splitAt 32
                  $ pkcs5_pbkdf2_hmac_sha1 password salt iters 48

    -- BS8.putStrLn $ "key=" <> Base16.encode key
    -- BS8.putStrLn $ "iv= " <> Base16.encode iv

    cipher <- getCipherByName "aes-256-cbc" >>= maybe (fail "no cipher") return
    plain <- cipherBS cipher key iv Decrypt enc

    return plain

type ExampleAPI = Get '[JSON] [String]

exampleAPI :: Proxy ExampleAPI
exampleAPI = Proxy

exampleServer :: String -> Server ExampleAPI
exampleServer msg = return ["hello", "world", msg]

main :: IO ()
main = withOpenSSL $ do
    password <- getEnv "PASSWORD" >>= maybe (fail "PASSWORD not set") return
    plain <- extract password encrypted'
    putStrLn "http://localhost:8000"
    hFlush stdout
    Warp.run 8000 $ serve exampleAPI $ exampleServer $ BS8.unpack plain
