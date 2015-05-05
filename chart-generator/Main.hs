module Main where

import Control.Applicative
import Data.ByteString.Lazy.Char8 (ByteString)
import Graphics.Rendering.Chart.Backend.Diagrams
import System.Environment
import System.Exit
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Csv as CSV

main :: IO ()
main = getArgs >>= go

go :: [String] -> IO ()
go = \case
  filename : xs -> do
    Right csv <- CSV.decode CSV.HasHeader <$> ByteString.readFile filename
    case xs of
      _ -> return ()
  _ -> do
    putStrLn "Invalid arguments"
    exitFailure
