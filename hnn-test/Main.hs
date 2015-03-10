module Main where

import Data.Classifier (Classifier)
import Data.Counter (Counter)
import Data.List (sortBy)
import Data.Text (Text)
import Data.Vector (Vector)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified Data.Classifier.NaiveBayes as Classifier
import qualified Data.Counter as Counter
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified System.IO.Strict as Strict

main :: IO ()
main = getArgs >>= \case
  ["network", infile] ->
    go infile
  _ -> do
    putStrLn "Invalid arguments"
    exitFailure

go :: FilePath -> IO ()
go infile = do
  file <- Strict.readFile infile

classifierToVector :: Classifier a b -> [(a, Vector b)]
classifierToVector (Classifier.toMap -> m) =
  []
