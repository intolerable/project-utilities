module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Char
import Data.Classifier (Classifier)
import Data.Classifier.NaiveBayes (NaiveBayes)
import Data.Counter (Counter)
import Data.Maybe
import Data.Monoid
import Data.STRef
import Data.Text (Text)
import Numeric.LinearAlgebra.HMatrix (Vector)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Random (randomR, getStdRandom, RandomGen)
import Text.Read (readMaybe)
import qualified AI.HNN.FF.Network as Neural
import qualified Data.Classifier as Classifier
import qualified Data.Classifier.NaiveBayes as NaiveBayes
import qualified Data.Counter as Counter
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Numeric.LinearAlgebra.HMatrix as Vector
import qualified System.IO as IO

main :: IO ()
main = getArgs >>= \case
  ["bayes", filename] ->
    readFile filename >>= print . bayes
  ["neural", filename, (readMaybe -> Just trainTimes), (readMaybe -> Just layers)] ->
    neural filename trainTimes layers
  _ -> do
    putStrLn "Invalid arguments"
    exitFailure

bayes :: String -> Counter (Bool, Maybe Bool)
bayes file = Counter.fromList results
  where res = extractData file
        results = applyNaiveBayes (createClassifier res) res

neural :: FilePath -> Int -> [Int] -> IO ()
neural path times layers = do
  shuffled <- readFile path >>= getStdRandom . shuffle . extractData
  let (train, test) = splitAt (length shuffled `div` 2) shuffled
  let trainClassifier = mconcat $ map rowToClassifier train
  let vocab = vocabulary trainClassifier
  let trainVectors = classifierToVector boolToVector vocab trainClassifier
  let testVectors = classifierToVector boolToVector vocab $ mconcat $ map rowToClassifier test
  case trainVectors of
    [] -> putStrLn "No data"
    (v, _) : _ -> do
      network <- Neural.createNetwork (Vector.size v) layers 1
      let result = Neural.trainNTimes times 0.8 Neural.tanh Neural.tanh' network trainVectors
      let trials = map (\(x, y) -> (y, Vector.cmap r $ Neural.output result tanh x)) testVectors
      IO.hSetBuffering IO.stdout IO.NoBuffering
      print $ Counter.fromList trials
      let result2 = Neural.trainNTimes (times * 10) 0.8 Neural.tanh Neural.tanh' network trainVectors
      let trials2 = map (\(x, y) -> (y, Vector.cmap r $ Neural.output result2 tanh x)) testVectors
      print $ Counter.fromList trials2
  where r = (fromIntegral :: Int -> Double) . (round :: Double -> Int)

boolToVector :: Bool -> Vector Double
boolToVector True = 1
boolToVector False = -1

vocabulary :: Ord b => Classifier a b -> Counter b
vocabulary = Map.foldr (mappend . mconcat) mempty . Classifier.toMap

counterToVector :: Ord a => Counter a -> Counter a -> Vector Double
counterToVector (Counter.toMap -> vocab) (Counter.toMap -> m) =
  Vector.vector $ map snd $ Map.toAscList $ Map.mergeWithKey (\_ v _ -> Just $ fromIntegral v) (const mempty) (fmap (const 0)) m vocab

classifierToVector :: (Ord a, Ord b) => (a -> Vector Double) -> Counter b -> Classifier a b -> [(Vector Double, Vector Double)]
classifierToVector f vocab (Classifier.toMap -> m) =
  Map.foldrWithKey (\k v a -> fmap ((,) <$> counterToVector vocab <*> pure (f k)) v <> a) [] m

applyNaiveBayes :: NaiveBayes Bool Text -> [Row] -> [(Bool, Maybe Bool)]
applyNaiveBayes classifier rows =
  foldl (\ a t -> collect (NaiveBayes.remove (rowToClassifier t) classifier) a t) [] rows

collect :: NaiveBayes Bool Text -> [(Bool, Maybe Bool)] -> Row -> [(Bool, Maybe Bool)]
collect cls acc (b, _, _, _, c) =
  (b, tested) : acc
  where tested = NaiveBayes.test cls $ Counter.fromList $ process c

type Row = (Bool, Text, Text, Text, Text)

extractData :: String -> [Row]
extractData = mapMaybe readMaybe . lines

createClassifier :: [Row] -> NaiveBayes Bool Text
createClassifier = mconcat . map (NaiveBayes.fromClassifier . rowToClassifier)

rowToClassifier :: Row -> Classifier Bool Text
rowToClassifier (b, _, _, _, c) = Classifier.singleton b $ Counter.fromList $ process c

for :: [a] -> (a -> b) -> [b]
for = flip map

genTable :: (Show a, Show b) => Classifier a b -> Text
genTable _ = Text.empty

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
