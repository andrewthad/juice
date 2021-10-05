import Control.Monad (when)
import System.Directory (createDirectoryIfMissing)
import Data.Compact (compactSized, compactSize)
import Data.Foldable (foldlM)

import qualified Json
import qualified Juice
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Chunks as Chunks
import qualified Data.Bytes.Builder as Builder

main :: IO ()
main = do
  measure "samples/hawaii.json"
  measure "samples/amazon-ip-ranges.json"
  measure "samples/twitter-100.json"

measure :: String -> IO ()
measure path = do
  b <- Bytes.readFile path
  val <- case Json.decode b of
    Left _ -> fail (path ++ " had invalid json")
    Right x -> pure x
  let cmpr = Juice.compress val
  let encJson = Builder.run 4080 (Json.encode val)
  let encJuice = Builder.run 4080 (Juice.encode cmpr)
  putStrLn path
  putStrLn ("  Minified Encoded JSON: " ++ show (Chunks.length encJson) ++ " bytes")
  putStrLn ("  Encoded Juice: " ++ show (Chunks.length encJuice) ++ " bytes")
  heapJson <- estimateHeapUse val
  heapCmpr <- estimateHeapUse cmpr
  putStrLn ("  JSON Heap Use: " ++ show heapJson ++ " bytes")
  putStrLn ("  Juice Heap Use: " ++ show heapCmpr ++ " bytes")

estimateHeapUse :: a -> IO Word
estimateHeapUse a = foldlM
  ( \lo i -> do
    w <- compactSized (i * 2000) True a >>= compactSize
    pure (min lo w)
  ) maxBound [1..50]
