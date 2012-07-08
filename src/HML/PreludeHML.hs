module HML.Prelude where

import Control.Parallel
import Control.Parallel.Strategies
import Data.List

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
