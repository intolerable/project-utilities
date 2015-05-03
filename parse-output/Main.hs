module Main where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text.Lazy
import Data.Counter (Counter)
import Data.Csv (ToRecord, Record)
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Read
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Counter as Counter
import qualified Data.Csv as CSV
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy.IO as Text
import qualified Data.Vector as Vector
import Data.ByteString.Lazy.Char8 (ByteString)

data Calc = Neural NeuralCalc
          | Naive NaiveCalc
  deriving (Show, Read, Eq)

instance ToRecord Calc where
  toRecord (Neural c) = CSV.toRecord c
  toRecord (Naive c) = CSV.toRecord c

data NeuralCalc = NeuralCalc [Int] Int Double Double (Counter Res)
  deriving (Show, Read, Eq)

instance ToRecord NeuralCalc where
  toRecord (NeuralCalc ls t ct tt c) = CSV.record
    [ "neural"
    , CSV.toField $ show ls
    , CSV.toField t
    , CSV.toField ct
    , CSV.toField tt ] <>
    resToRecord c

resToRecord :: Counter Res -> Record
resToRecord c = CSV.record
  $ map (\x -> CSV.toField $ Counter.lookup x c) [TruePositive .. UnknownNegative] <> [CSV.toField $ Counter.total c]

data NaiveCalc = NaiveCalc Double Double (Counter Res)
  deriving (Show, Read, Eq)

instance ToRecord NaiveCalc where
  toRecord (NaiveCalc ct tt c) = CSV.record
    [ "naive"
    , mempty
    , mempty
    , CSV.toField ct
    , CSV.toField tt ] <>
    resToRecord c

data Res = TruePositive
         | TrueNegative
         | FalsePositive
         | FalseNegative
         | UnknownPositive
         | UnknownNegative
  deriving (Show, Read, Eq, Ord, Enum)

boolsToRes :: (Bool, Maybe Bool) -> Res
boolsToRes x = case x of
  (True, Just True) -> TruePositive
  (False, Just False) -> TrueNegative
  (False, Just True) -> FalsePositive
  (True, Just False) -> FalseNegative
  (True, Nothing) -> UnknownPositive
  (False, Nothing) -> UnknownNegative

vectToRes :: (Vector Double, Vector Double) -> Res
vectToRes (x, y) = case (Vector.toList x, Vector.toList y) of
  ([1], [1]) -> TruePositive
  ([-1], [-1]) -> TrueNegative
  ([-1], [1]) -> FalsePositive
  ([1], [-1]) -> FalseNegative
  ([1], [0]) -> UnknownPositive
  ([-1], [0]) -> UnknownNegative
  _ -> error "invalid result"

main :: IO ()
main = getArgs >>= \case
  [filename] -> go filename >>= ByteString.putStrLn
  x -> do
    putStrLn "Invalid arguments:"
    print x
    exitFailure

go :: FilePath -> IO ByteString
go filename = do
  file <- Text.readFile filename
  case eitherResult $ parse resultFile file of
    Left err -> do
      print err
      exitFailure
    Right xs ->
      return $ CSV.encode xs

resultFile :: Parser [Calc]
resultFile = many ((Neural <$> neuralCalc) <|> (Naive <$> naiveCalc))

neuralCalc :: Parser NeuralCalc
neuralCalc = do
  layers <- label "layers" $ ss $ lineRemainder >>= readParser
  train <- label "train" $ ss decimal
  label "network" $ skipMany $
    (("Network" <|> "[" <|> ",") *> ss lineRemainder)
  ct <- label "ct" $ ss timingLine
  results <- label "results" $ ss lineRemainder >>= readParser
  ft <- label "ft" $ ss timingLine
  return $ NeuralCalc layers train ct ft (mapKey vectToRes results)

lineRemainder :: Parser Text
lineRemainder = ss $ takeTill isEndOfLine

label :: String -> Parser a -> Parser a
label s x = x <?> s

mapKey :: (Ord a, Ord b) => (a -> b) -> Counter a -> Counter b
mapKey f (Counter.toMap -> m) =
  Counter.fromMap $ Map.mapKeys f m

naiveCalc :: Parser NaiveCalc
naiveCalc = do
  void $ ss $ lineRemainder
  ct <- ss timingLine
  results <- ss $ lineRemainder >>= readParser
  ft <- ss timingLine
  return $ NaiveCalc ct ft (mapKey boolsToRes results)

readParser :: Read a => Text -> Parser a
readParser x = case readMaybe (Text.unpack x) of
  Just y -> return y
  Nothing -> fail "read failed"

timingLine :: Parser Double
timingLine = double <* "s"

ss :: Parser a -> Parser a
ss x = x <* skipSpace
