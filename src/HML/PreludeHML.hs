module PreludeHML where

import Control.Parallel
import Control.Parallel.Strategies
--import Data.List 
import Data.Packed.Vector (Vector)
import qualified Data.Sequence as DS  
import Data.Sequence (Seq,zip,splitAt)
import Prelude hiding (splitAt,zip)
import Data.Foldable

data SupervisedExperiment = SupExp { training_set  :: Seq (Vector Double,Double),
                                     test_set      :: Seq (Vector Double,Double),
                                     learning_rate :: Double,
                                     iterations    :: Int
                                   }

jobSize = 1000000

mse :: Seq Double -> Seq Double -> Double
mse a b = (parMSE (zip a b)) / 2.0


parMSE s = do
  case DS.viewl s of
    DS.EmptyL    -> 0.0
    otherwise -> p `par` q `pseq` p+q
  where p = parMSE right
        q = foldl' (\a (x,y) -> a + (x-y)*(x-y)) 0.0 left
        (left,right) = splitAt jobSize s