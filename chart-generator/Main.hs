module Main where

import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import Graphics.Rendering.Chart.Backend.Diagrams
import System.Environment
import System.Exit
import Data.Map (Map)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Csv as CSV
import qualified Data.Vector as Vector
import qualified Data.Map as Map

main :: IO ()
main = getArgs >>= go

go :: [String] -> IO ()
go = \case
  filename : xs -> do
    Right (headers, csv) <- CSV.decodeByName <$> ByteString.readFile filename
    case xs of
      _ -> charts (Vector.toList headers) (Vector.toList csv)
  _ -> do
    putStrLn "Invalid arguments"
    exitFailure

charts :: [ByteString] -> [Map ByteString ByteString] -> IO ()
charts = undefined
