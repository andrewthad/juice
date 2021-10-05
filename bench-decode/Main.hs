{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Gauge.Main (defaultMain,bgroup,bench,whnfAppIO)
import Control.Exception (throwIO)
import Data.Bytes (Bytes)

import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Builder as BLDR
import qualified Data.Bytes.Chunks as Chunks
import qualified Data.Bytes.Builder as Builder
import qualified Json
import qualified Juice

main :: IO ()
main = do
  (hawaiiJson,hawaiiJuice) <- loadAndJuice "samples/hawaii.json"
  (amazonJson,amazonJuice) <- loadAndJuice "samples/amazon-ip-ranges.json"
  (twitterJson,twitterJuice) <- loadAndJuice "samples/twitter-100.json"
  defaultMain
    [ bgroup "hawaii"
      [ bench "json" $ whnfAppIO (\b -> either throwIO pure (Json.decode b)) hawaiiJson
      , bench "juice" $ whnfAppIO (\b -> maybe (fail "hawaii-json-no-decode") pure (Juice.decode b)) hawaiiJuice
      , bench "juice-decompress" $ whnfAppIO
          (\b -> maybe (fail "hawaii-json-no-decode") (pure . Juice.decompress) (Juice.decode b)
          ) hawaiiJuice
      ]
    , bgroup "amazon-ip-ranges"
      [ bench "json" $ whnfAppIO (\b -> either throwIO pure (Json.decode b)) amazonJson
      , bench "juice" $ whnfAppIO (\b -> maybe (fail "amazon-json-no-decode") pure (Juice.decode b)) amazonJuice
      , bench "juice-decompress" $ whnfAppIO
          (\b -> maybe (fail "amazon-json-no-decode") (pure . Juice.decompress) (Juice.decode b)
          ) amazonJuice
      ]
    , bgroup "twitter-100"
      [ bench "json" $ whnfAppIO (\b -> either throwIO pure (Json.decode b)) twitterJson
      , bench "juice" $ whnfAppIO (\b -> maybe (fail "twitter-json-no-decode") pure (Juice.decode b)) twitterJuice
      , bench "juice-decompress" $ whnfAppIO
          (\b -> maybe (fail "twitter-json-no-decode") (pure . Juice.decompress) (Juice.decode b)
          ) twitterJuice
      ]
    ]

loadAndJuice :: String -> IO (Bytes, Bytes)
loadAndJuice filename = do
  bytes <- Bytes.readFile filename
  case Json.decode bytes of
    Left _ -> fail "loadAndJuice: failed to decode"
    Right v -> do
      let !j = Chunks.concat (Builder.run 4096 (Juice.encode (Juice.compress v)))
      pure (bytes,j)
