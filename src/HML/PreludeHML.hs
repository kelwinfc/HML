module HML.PreludeHML where

import Control.Parallel
import Control.Parallel.Strategies
import qualified Data.List as DL
import Data.Packed.Vector (Vector)
import qualified Data.Sequence as DS  
import Data.Sequence (Seq,splitAt)
import Prelude hiding (splitAt,zip)
import Data.Foldable
import Graphics.Gnuplot.Simple
import qualified Data.Foldable as DF


data SupervisedExperiment = SupExp { training_set  :: Seq (Vector Double,Double),
                                     test_set      :: Seq (Vector Double,Double),
                                     learning_rate :: Double,
                                     regularization_parameter :: Double,
                                     iterations    :: Int
                                   }

data MultivalueSupExp = MSupExp { training      :: [ ([Double],[Double]) ],
                                  test          :: [ ([Double],[Double]) ],
                                  alpha         :: Double,
                                  max_it        :: Int
                                }

class MLPredictor a where
    (~>)  :: [Double] -> a -> [Double]

data ConstM = CM

instance MLPredictor ConstM where
    (~>) entries CM = map (*2) entries

jobSize = 1000000

mse :: Seq Double -> Seq Double -> Double
mse a b = (foldl' (\a (x,y) -> a + (x-y)*(x-y)) 0.0 (DS.zip a b)) 
            / ( 2.0 * (fromIntegral $ DS.length a))

parMSE s = do
  case DS.viewl s of
    DS.EmptyL    -> 0.0
    otherwise -> p `par` q `pseq` p+q
  where p = parMSE right
        q = foldl' (\a (x,y) -> a + (x-y)*(x-y)) 0.0 left
        (left,right) = splitAt jobSize s

mseMatrix :: [[Double]] -> [[Double]] -> Double
mseMatrix a b = (DL.foldl' (+) 0 (zipWith (\x y -> mse (DS.fromList x)
                                                       (DS.fromList y))
                                           a b))
                   / (2.0 * (fromIntegral $ length a))

plotStats :: String -> DS.Seq (Double, Double) -> IO()
plotStats pathfile stats = do 
    plotPaths [PNG pathfile, XLabel "Iterations", YLabel "MSE"]
              [tr, ts]
    where tr = toLine $ fmap fst stats
          ts = toLine $ fmap snd stats
          toLine :: DS.Seq Double -> [(Double,Double)]
          toLine s = DL.zip [1.0 .. fromIntegral $ DS.length s] (DF.toList s)

getTrainingAndTestSets :: [a] -> Int -> ([a],[a])
getTrainingAndTestSets [] _ = ([],[])
getTrainingAndTestSets a k = (ff ++ (fst next), sf ++ (snd next))
    where (f,s)   = DL.splitAt (k+1) a
          (ff,sf) = (take 1 f, take k (tail f))
          next    = getTrainingAndTestSets s k
