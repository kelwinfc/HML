module LinearRegression where

import Control.Monad.RWS hiding (join)
import Control.Parallel
import Data.Sequence (Seq)
import qualified Data.Sequence as DS
import Data.Packed.Vector
--import Data.Packed.Matrix
import Numeric.Container ((<.>))
import qualified Numeric.Container as NC
import qualified Data.Foldable as DF
import qualified Data.Functor as DR

import PreludeHML

hypothesis :: Vector Double    -- theta
              -> Vector Double -- x
              -> Double        -- h_theta(x)
hypothesis theta x = theta <.> x

derivedJtheta :: (Vector Double -> Vector Double -> Double) -- hypothesis function
                 -> Seq (Vector Double,Double)              -- training set
                 -> Vector Double                           -- theta
                 -> Int                                     -- index j to calculate
                 -> Double                                  -- value of derived cost function respect to theta_j 
derivedJtheta h tr th j =  (DF.sum $ DR.fmap (e th j) tr) / (toEnum m)
   where m = DS.length tr
         e t j (x,y) = ((h t x) - y) * (x @> j)

--type LinearRegressionMonad = RWST SupervisedExperiment (Seq (Double,Double)) (Vector Double,Int) IO ()
type LinearRegressionMonad = RWS SupervisedExperiment (Seq (Double,Double)) (Vector Double,Int) ()

training :: LinearRegressionMonad
training = do
  data_training <- ask
  (theta,i) <- get
  let it = iterations data_training
  let trs = DR.fmap one (training_set data_training)
  let tss = DR.fmap one (test_set data_training)
  let alpha = learning_rate data_training
  let parameters =  calculate_parameters alpha trs theta 0
  let trs_get = DR.fmap (h parameters) trs
  let tss_get = DR.fmap (h parameters) tss
  let h_trs = DR.fmap fst trs_get
  let y_trs = DR.fmap snd trs_get
  let h_tss = DR.fmap fst tss_get
  let y_tss = DR.fmap snd tss_get
  tell $ DS.singleton (mse h_trs y_trs,mse h_tss y_tss)
  put (parameters, i + 1)
  if i == it
    then do
      return ()
    else
      training 
    where h th (x,y) = (hypothesis th x,y)

-- gradientDescent :: Double                         -- alpha
--                    -> Seq (Vector Double, Double) -- training set
--                    -> Vector Double               -- initial theta value
--                    -> Int                         -- iterations
--                    -> Vector Double               -- parameters and graphics
-- gradientDescent _ _ th 0      = th
-- gradientDescent alpha tr th i = gradientDescent alpha tr (calculate_parameters alpha tr th 0) (i - 1)

one :: (Vector Double, Double) -> (Vector Double, Double) 
one (x,y) = (join [fromList [1],x],y)

calculate_theta :: Double                        -- previous theta value
                   -> Double                     -- alpha                                                
                   -> Seq (Vector Double,Double) -- training set
                   -> Vector Double              -- previous theta set
                   -> Int                        -- index j to calculate
                   -> Double                     -- new theta
calculate_theta th_pr alpha tr th j = 
  th_pr - (alpha * (derivedJtheta hypothesis tr th j))
  
calculate_parameters :: Double                        -- alpha
                        -> Seq (Vector Double,Double) -- training set
                        -> Vector Double              -- previous theta set
                        -> Int                        -- initial j
                        -> Vector Double              -- new theta set
calculate_parameters alpha tr th_pr j = 
  if j == n 
  then (fromList[calculate_theta  (th_pr @> j) alpha tr th_pr j])
  else (th_j `par` sub_th `pseq` (join [fromList[th_j],sub_th]))
  where  n    = dim th_pr - 1
         th_j = calculate_theta (th_pr @> j) alpha tr th_pr j
         sub_th = calculate_parameters alpha tr th_pr (j + 1) 

is_theta :: Vector Double
is_theta = fromList [1,1]
x1 :: Vector Double
x1 = fromList [3]
x2 :: Vector Double
x2 = fromList [4]
x3 :: Vector Double
x3 = fromList [5]
y1 = 4.0
y2 = 5.0
y3 = 6.0
training__set = DS.fromList [(x1,y1),(x2,y2),(x3,y3)]
is_alpha = 0.01