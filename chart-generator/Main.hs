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

chart1 :: [ByteString] -> [Map ByteString ByteString] -> Layout LogValue Percent
chart1 hs es =
  def & layout_plots .~ plots
      & layout_all_font_styles.font_size *~ 2
  where
    relevants = filter (\m -> m ! "layers" == "[]") es
    --field :: ByteString -> PlotLines LogValue Double
    field x c =
      def
        & plot_lines_values .~
          [map (\m -> (LogValue $ bread (m ! "train epochs"), Percent $ bread (m ! x))) relevants]
        & plot_lines_style .~ solidLine 5 (opaque c)
        & plot_lines_title .~ ByteString.unpack x
    plots =
      [ toPlot $ field "precision" green
      , toPlot $ field "recall" red ]
    bread = read . ByteString.unpack

(!) :: Ord a => Map a b -> a -> b
m ! k = case Map.lookup k m of
  Just x -> x
  Nothing -> error "Missing key"

setLinesBlue :: PlotLines a b -> PlotLines a b
setLinesBlue = plot_lines_style  . line_color .~ opaque blue

