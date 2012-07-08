module HML.PreludeHML where

import Control.Parallel
import Control.Parallel.Strategies

import Data.List
import qualified Data.Sequence as DS
import qualified Data.Foldable as DF

import Graphics.Gnuplot.Simple

data SupervisedExperiment = SupExp { training_set  :: [(Double,Double)],
                                     test_set      :: [(Double,Double)],
                                     learning_rate :: Double,
                                     iterations    :: Int
                                   }

jobSize = 1000000

mse :: [Double] -> [Double] -> Double
mse a b = (parMSE (zip a b)) / 2.0

parMSE [] = 0.0
parMSE l = p `par` q `pseq` p+q
    where p = parMSE right
          q = foldl' (\a (x,y) -> a + (x-y)*(x-y)) 0.0 left
          (left,right) = splitAt jobSize l

plotStats :: String -> DS.Seq (Double, Double) -> IO()
plotStats pathfile stats = do 
    plotPaths [PNG pathfile, XLabel "Iterations", YLabel "MSE"]
              [tr, ts]
    where tr = toLine $ fmap fst stats
          ts = toLine $ fmap snd stats
          toLine :: DS.Seq Double -> [(Double,Double)]
          toLine s = zip [1.0 .. fromIntegral $ DS.length s] (DF.toList s)
