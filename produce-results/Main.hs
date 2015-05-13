module Main (main) where

import Autoencoder

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
import Data.Time.Clock
import Numeric.LinearAlgebra.HMatrix (Vector)
import Prelude
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
    void $ evaluateBayes filename
  ["neural", filename, readMaybe -> Just trainTimes, readMaybe -> Just layers] ->
    neural filename trainTimes layers
  ["generate_autoencoder", filename] -> do
    IO.hSetBuffering IO.stdout IO.NoBuffering
    t <- getCurrentTime
    autoencoder filename >>= print
    getCurrentTime >>= print . (`diffUTCTime` t)
  ["apply_autoencoder", encoder, filename] -> do
    (v, (e, _)) <- read <$> readFile encoder :: IO (Counter Text, Autoencoder)
    applyAutoencoder v e filename 10 []
  ["everything", filename] -> do
    void $ evaluateBayes filename
    neural filename 10 []
    neural filename 100 []
    neural filename 1000 []
    neural filename 10 [10]
    neural filename 100 [10]
    neural filename 1000 [10]
    neural filename 10 [100]
    neural filename 100 [100]
    neural filename 1000 [100]
    neural filename 10000 [10]
    neural filename 10 [100, 50]
    neural filename 100 [100, 50]
    neural filename 1000 [100, 50]
  _ -> do
    putStrLn "Invalid arguments"
    exitFailure

applyAutoencoder :: Counter Text -> Encoder -> FilePath -> Int -> [Int] -> IO ()
applyAutoencoder vocab encoder path times layers = do
  shuffled <- readFile path >>= getStdRandom . shuffle . extractData
  let (train, test) = splitAt (length shuffled `div` 2) shuffled
  let trainVectors = map (\(x, y) -> (encode encoder x, y)) $ classifierToVector boolToVector vocab $ mconcat $ map rowToClassifier train
  let testVectors = map (\(x, y) -> (encode encoder x, y)) $ classifierToVector boolToVector vocab $ mconcat $ map rowToClassifier test
  case trainVectors of
    [] -> putStrLn "No data"
    (v, _) : _ -> do
      startTime <- getCurrentTime
      network <- Neural.createNetwork (Vector.size v) layers 1
      IO.hSetBuffering IO.stdout IO.NoBuffering
      void $ iterateM 9 (0 :: Int, network, 0) $ \(n, net, timeTakenEvaluating) -> do
        let newNet = trainNet net trainVectors
        print layers
        print (n + times)
        print newNet
        endTime <- getCurrentTime
        print $ (endTime `diffUTCTime` startTime) - timeTakenEvaluating
        evalStartTime <- getCurrentTime
        print $ Counter.fromList $ map (\(x, y) -> (y, Vector.cmap r $ Neural.output newNet tanh x)) testVectors
        evalEndTime <- getCurrentTime
        print $ evalEndTime `diffUTCTime` evalStartTime
        return (n + times, newNet, timeTakenEvaluating + (evalEndTime `diffUTCTime` evalStartTime))
  where r = (fromIntegral :: Int -> Double) . (round :: Double -> Int)
        trainNet = Neural.trainNTimes times 0.8 Neural.tanh Neural.tanh'

autoencoder :: FilePath -> IO (Counter Text, Autoencoder)
autoencoder path = do
  shuffled <- readFile path >>= getStdRandom . shuffle . extractData
  let (train, test) = splitAt (length shuffled `div` 2) shuffled
  let trainClassifier = mconcat $ map rowToClassifier train
  let vocab = vocabulary trainClassifier
  let trainVectors = classifierToVector boolToVector vocab trainClassifier
  let testVectors = classifierToVector boolToVector vocab $ mconcat $ map rowToClassifier test
  case trainVectors of
    [] -> error "no data"
    v -> do
      res <- generateAutoencoderIO (map fst v) 1000 1000
      return (vocab, res)

evaluateBayes :: FilePath -> IO (Counter (Bool, Maybe Bool))
evaluateBayes fp = do
  file <- readFile fp
  startTime <- getCurrentTime
  let res = extractData file
  let classifier = createClassifier res
  print classifier
  printTimeDiff startTime
  evalStartTime <- getCurrentTime
  let counted = Counter.fromList $ applyNaiveBayes classifier res
  print counted
  printTimeDiff evalStartTime
  return counted

printTimeDiff :: UTCTime -> IO ()
printTimeDiff s = do
  e <- getCurrentTime
  print $ e `diffUTCTime` s

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
      startTime <- getCurrentTime
      network <- Neural.createNetwork (Vector.size v) layers 1
      IO.hSetBuffering IO.stdout IO.NoBuffering
      void $ iterateM 9 (0 :: Int, network, 0) $ \(n, net, timeTakenEvaluating) -> do
        let newNet = trainNet net trainVectors
        print layers
        print (n + times)
        print newNet
        endTime <- getCurrentTime
        print $ (endTime `diffUTCTime` startTime) - timeTakenEvaluating
        evalStartTime <- getCurrentTime
        print $ Counter.fromList $ map (\(x, y) -> (y, Vector.cmap r $ Neural.output newNet tanh x)) testVectors
        evalEndTime <- getCurrentTime
        print $ evalEndTime `diffUTCTime` evalStartTime
        return (n + times, newNet, timeTakenEvaluating + (evalEndTime `diffUTCTime` evalStartTime))
  where r = (fromIntegral :: Int -> Double) . (round :: Double -> Int)
        trainNet = Neural.trainNTimes times 0.8 Neural.tanh Neural.tanh'

iterateM :: Monad m => Int -> a -> (a -> m a) -> m [a]
iterateM 0 _ _ = return []
iterateM n a f = do
  v <- f a
  next <- iterateM (n - 1) v f
  return $ v : next

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

process :: Text -> [Text]
process = filter (not . Text.null) .
          map (Text.map toLower . Text.filter isAlpha) .
          concatMap (Text.splitOn ".") .
          Text.splitOn " " .
          Text.filter (not . (== '-'))

-- shuffle from the haskell wiki @ https://wiki.haskell.org/Random_shuffle
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
