{-# language LambdaCase #-}

import Control.Monad (when)
import Data.Compact (compactSized, compactSize)
import Data.Foldable (foldlM)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)

import qualified Json
import qualified Juice
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Chunks as Chunks
import qualified Data.Bytes.Builder as Builder
import qualified System.IO as IO

main :: IO ()
main = do
  cs <- getArgs >>= \case
    [path] -> Chunks.readFile path
    [] -> do
      r <- Chunks.hGetContents IO.stdin
      when (Chunks.null r) (fail "empty input stream")
      pure r
    _ -> fail "expected zero or one filenames as argument" 
  let b = Chunks.concat cs
  val <- case Json.decode b of
    Left _ -> fail "invalid json"
    Right x -> pure x
  let cmpr = Juice.compress val
  let encMinified = Builder.run 4080 (Json.encode val)
  let encJuice = Builder.run 4080 (Juice.encode cmpr)
  putStrLn ("Original Size: " ++ show (Bytes.length b))
  putStrLn ("Minified Size: " ++ show (Chunks.length encMinified))
  putStrLn ("Juice Size:    " ++ show (Chunks.length encJuice))
  Chunks.writeFile "out.juice" encJuice
