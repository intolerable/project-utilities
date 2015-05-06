module Main where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.Default.Class
import Data.Colour
import Data.Colour.Names
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams
import System.Environment
import System.Exit
import Data.Map (Map)
import qualified Data.ByteString.Lazy.Char8 as Lazy
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Csv as CSV
import qualified Data.Vector as Vector
import qualified Data.Map as Map
import Debug.Trace

ts x = traceShow x x

main :: IO ()
main = getArgs >>= go

go :: [String] -> IO ()
go = \case
  filename : xs -> do
    Right (headers, csv) <- CSV.decodeByName <$> Lazy.readFile filename
    case xs of
      _ -> charts (Vector.toList headers) (Vector.toList csv)
  _ -> do
    putStrLn "Invalid arguments"
    exitFailure

charts :: [ByteString] -> [Map ByteString ByteString] -> IO ()
charts hs es = do
  void $ renderableToFile (def & fo_format .~ EPS) "../document-project/images/chart1.eps" (toRenderable $ chart1 hs es)
  void $ renderableToFile (def & fo_format .~ SVG) "../document-project/images/chart1.svg" (toRenderable $ chart1 hs es)

chart1 :: [ByteString] -> [Map ByteString ByteString] -> Layout Double Double
chart1 hs es =
  def & layout_plots .~ plots
  where
    relevants = filter (\m -> m ! "layers" == "[]") es
    field :: ByteString -> PlotPoints Double Double
    field x =
      def
        & plot_points_values .~
          map (\x -> (fromIntegral x, 2)) [1..10]
        & plot_points_style .~ (def & point_radius .~ 0.5)
        -- & plot_lines_style .~ solidLine 0. (opaque blue)
    plots =
      [ toPlot $ field "true positive"
      , toPlot $ field "true negative"
      , toPlot $ field "false positive"
      , toPlot $ field "false negative" ]
    --bread = read . ByteString.unpack

(!) :: Ord a => Map a b -> a -> b
m ! k = case Map.lookup k m of
  Just x -> x
  Nothing -> error "Missing key"
