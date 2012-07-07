--import Control.Monad.RWS
import Data.Sequence (Seq)
import qualified Data.Sequence as DS
import Data.Packed.Vector (Vector,(@>))
import Data.Packed.Matrix
import Numeric.Container ((<.>))
import qualified Numeric.Container as NC
import qualified Data.Foldable as DF
import qualified Data.Functor as DR

hypothesis :: Vector Double    -- x
              -> Vector Double -- theta
              -> Double        -- h_theta(x)
hypothesis theta x = theta <.> x

derivedJtheta :: (Vector Double -> Vector Double -> Double) -- hypothesis function
                 -> Seq (Vector Double,Double)              -- training set
                 -> Vector Double                           -- theta
                 -> Int                                     -- index j to calculate
                 -> Double                                  -- value of derived cost function respect to theta_j 
derivedJtheta h tr th j =  (DF.sum $ DR.fmap (e th j) tr) / (toEnum m)
   where m = DS.length tr
         e t j (x,y) = ((h x t) - y) * (x @> j)

-- training :: Seq (Vector Double, Double) -- training set
--              -> Double                  -- learning rate
--              -> Vector Double           -- initial values of theta
--              -> Vector Double           -- final values of theta
-- training tr alpha th_ini = 