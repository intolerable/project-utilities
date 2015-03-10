module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Char
import Data.Classifier (Classifier)
import Data.Maybe
import Data.Monoid
import Data.STRef
import Data.Text (Text)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Random (randomR, getStdRandom, RandomGen)
import Text.Read (readMaybe)
import qualified Data.Classifier.NaiveBayes as Classifier
import qualified Data.Counter as Counter
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified System.IO as IO

main :: IO ()
main = getArgs >>= \case
  ["table", filename] ->
    void $ table filename
  _ -> do
    putStrLn "Invalid arguments"
    exitFailure

table :: FilePath -> IO ()
table path = do
  file <- readFile path
  let res = extractData file
  print $ length (Set.toList (Set.fromList res)) == length res
  shuffledRes <- getStdRandom $ shuffle $ Set.toList $ Set.fromList res
  let results = applyClassifier shuffledRes
  IO.hSetBuffering IO.stdout IO.NoBuffering
  print results
  print $ Counter.fromList results

applyClassifier :: [Row] -> [(Bool, Maybe Bool)]
applyClassifier rows = foldl (collect classifier) [] $ take 100 test
  where
    (train, test) = splitAt (length rows `div` 2) rows
    classifier = createClassifier train

collect :: Classifier Bool Text -> [(Bool, Maybe Bool)] -> Row -> [(Bool, Maybe Bool)]
collect cls acc (b, _, _, _, c) =
  (b, tested) : acc
  where tested = Classifier.test cls $ Counter.fromList $ process c

type Row = (Bool, Text, Text, Text, Text)

extractData :: String -> [Row]
extractData = mapMaybe readMaybe . lines

createClassifier :: [Row] -> Classifier Bool Text
createClassifier = mconcat . map rowToClassifier

rowToClassifier :: Row -> Classifier Bool Text
rowToClassifier (b, _, _, _, c) = Classifier.singleton b $ Counter.fromList $ process c

for :: [a] -> (a -> b) -> [b]
for = flip map

genTable :: (Show a, Show b) => Classifier a b -> Text
genTable _ = Text.empty

tableList :: (Show a, Show b, Ord a, Ord b) => Classifier a b -> [[Text]]
tableList c = [[tshow $ Classifier.priors c]]

tshow :: Show a => a -> Text
tshow = Text.pack . show

process :: Text -> [Text]
process = filter (not . Text.null) .
          map (Text.map toLower . Text.filter isAlpha) .
          concatMap (Text.splitOn ".") .
          Text.splitOn " " .
          Text.filter (not . (== '-'))

shuffle :: RandomGen g => [a] -> g -> ([a], g)
shuffle xs gen = runST $ do
    g <- newSTRef gen
    ar <- newListArray' xs
    xs' <- forM [1..n] $ \i -> do
            j <- randomRST (i,n) g
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
    gen' <- readSTRef g
    return (xs', gen')
  where
    n = length xs
    newListArray' :: [a] -> ST s (STArray s Int a)
    newListArray' = newListArray (1, n)
    randomRST lohi g = do
      (a, s') <- randomR lohi <$> readSTRef g
      writeSTRef g s'
      return a
