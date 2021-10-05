{-# language LambdaCase #-}

import Control.Monad (when)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)

import qualified Json
import qualified Juice
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Chunks as Chunks
import qualified Data.Bytes.Builder as Builder

main :: IO ()
main = do
  -- Use -d or --dump to dump out artifacts even on success.
  debug <- getArgs >>= \case
    [] -> pure False
    ["--dump"] -> pure True
    ["-d"] -> pure True
    _ -> fail "invalid flags"
  b <- Bytes.readFile "samples/hawaii.json"
  val <- case Json.decode b of
    Left _ -> fail "hawaii.json had invalid json"
    Right x -> pure x
  let cmpr = Juice.compress val
      val' = Juice.decompress cmpr
  when (val /= val') $ do
    createDirectoryIfMissing False "logs"
    Chunks.writeFile "logs/hawaii.json" (Builder.run 4080 (Json.encode val'))
    writeFile "logs/hawaii.before.txt" (show val)
    writeFile "logs/hawaii.after.txt" (show val')
    fail "compression of hawaii.json did not roundtrip, dumping to logs/hawaii.json"
  let encJuice = Builder.run 4080 (Juice.encode cmpr)
  when debug $ do
    Chunks.writeFile "logs/hawaii.juice" encJuice
  case Juice.decode (Chunks.concat encJuice) of
    Nothing -> fail "hawaii.json: could not decode compressed value after encoding it"
    Just cmpr' -> when (cmpr /= cmpr') $ do
      Chunks.writeFile "logs/hawaii.json" (Builder.run 4080 (Json.encode (Juice.decompress cmpr')))
      fail "hawaii.json: compression codec roundtrip failure, dumping to logs/hawaii.json"
  putStrLn "Success"
