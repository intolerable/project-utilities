module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import Data.Monoid
import Data.Text (Text)
import System.Environment
import System.Exit
import Text.Read
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified System.IO.Strict as Strict

main :: IO ()
main = getArgs >>= \case
  [infile, outfile] -> go infile outfile
  _ -> do
    putStrLn "Invalid arguments"
    exitFailure

go :: FilePath -> FilePath -> IO ()
go infile outfile = do
  file <- Strict.readFile infile
  res <- execWriterT (convert file)
  Text.writeFile outfile $ Text.intercalate "\n" res

convert :: String -> WriterT [Text] IO ()
convert file = do
  mapM_ output
    [ "% Automatically generated ARFF output from comment-to-arff"
    , "@relation comments"
    , ""
    , "@attribute relevance {Related,Unrelated}"
    , "@attribute id string"
    , "@attribute title string"
    , "@attribute username string"
    , "@attribute content string"
    , ""
    , "@data" ]
  forM_ (lines file `zip` [1..]) $ \(line, n) ->
    case readMaybe line :: Maybe (Bool, Text, Text, Text, Text) of
      Nothing ->
        liftIO $ putStrLn $ mconcat
            [ "Couldn't parse line ", show n, ":\n"
            , "  ", take 30 line, "..." ]
      Just (relevance, i, t, u, c) ->
        output $ Text.intercalate ","
          [ if relevance then "Related" else "Unrelated"
          , tshow i
          , tshow t
          , tshow u
          , tshow c ]
    where tshow = Text.pack . show

output :: Monad m => Text -> WriterT [Text] m ()
output = tell . return
