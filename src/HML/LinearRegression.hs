--import Control.Monad.RWS
import Data.Sequence (Seq)
import qualified Data.Sequence as DS
import Data.Packed.Vector (Vector,(@>))
import Data.Packed.Matrix
import Numeric.Container ((<.>))
import qualified Numeric.Container as NC
import qualified Data.Foldable as DF
import qualified Data.Functor as DR

hypothesis :: Vector Double -> Vector Double -> Double
hypothesis theta x = theta <.> x

derivedJtheta :: (Vector Double -> Vector Double -> Double) -> Seq (Vector Double,Double) -> Vector Double -> Int -> Double
derivedJtheta h tr th j =  (DF.sum $ DR.fmap ((th_j *) . (e th)) tr) / (toEnum m)
   where m = DS.length tr
         th_j = th @> j
         e t (x,y) = (h x t) - y