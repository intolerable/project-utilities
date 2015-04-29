module Autoencoder where

import AI.HNN.FF.Network (Network)
import Numeric.LinearAlgebra.HMatrix (Vector, Matrix)
import System.Exit
import qualified AI.HNN.FF.Network as Network
import qualified Data.Vector
import qualified Numeric.LinearAlgebra.HMatrix as Vector

newtype Encoder = Encoder (Matrix Double)
  deriving (Show, Read, Eq)

newtype Decoder = Decoder (Matrix Double)
  deriving (Show, Read, Eq)

type LayerCount = Int
type EpochCount = Int
type Autoencoder = (Encoder, Decoder)

generateAutoencoderIO :: [Vector Double] -> LayerCount -> EpochCount -> IO Autoencoder
generateAutoencoderIO v l e = do
  net <- Network.createNetwork (Vector.size $ head v) [l] (Vector.size $ head v)
  let Network.Network x = Network.trainNTimes e 0.8 Network.tanh Network.tanh' net (zipWith (,) v v)
  case Data.Vector.toList x of
    [enc, dec] -> return (Encoder enc, Decoder dec)
    _ -> error "colossal failure"

encode :: Encoder -> Vector Double -> Vector Double
encode (Encoder m) v = Vector.app m $ Vector.vjoin [v, 1]

decode :: Decoder -> Vector Double -> Vector Double
decode (Decoder m) v = Vector.app m $ Vector.vjoin [v, 1]
