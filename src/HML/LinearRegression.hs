module LinearRegression where

import Control.Monad.RWS hiding (join)
import Control.Parallel
import Data.Sequence (Seq)
import qualified Data.Sequence as DS
import Data.Packed.Vector
--import Data.Packed.Matrix
import Numeric.Container ((<.>),constant)
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

training :: LinearRegressionMonad
training = do
  data_training <- ask
  (theta,i) <- get
  let it = iterations data_training
  if it /= 0 && i < it 
    then do 
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
      training
    else return ()
  where h th (x,y) = (hypothesis th x,y)

one (x,y) = (join [fromList [1],x],y)


linearRegression :: Double                         -- learning rate
                    -> Seq (Vector Double, Double) -- training set
                    -> Seq (Vector Double, Double) -- test set
                    -> Int                         -- number of features
                    -> Int                         -- max number of iterations
                    -> IO ()
linearRegression alpha tr ts num_features i = do
  let se = SupExp {training_set = tr, test_set = ts, 
                   learning_rate = alpha, iterations = i}
  let initial_theta = constant 1 (num_features + 1)
  let (_,s,w) = runRWS training se (initial_theta,0)
  print $ fst s
  plotStats "Errors Graphics of Linear Regression" w
      
---------------- EXAMPLE

f x = (fromList [x],-2+3*x)
g x = (fromList [x],3+2*x)

prueba_tr :: Seq (Vector Double, Double)
prueba_tr = DR.fmap g $ DS.fromList [0.0,2.0,3.0,5.0]

prueba_th :: Vector Double
prueba_th = fromList [1,1]

training__set :: Seq (Vector Double, Double)
training__set = DR.fmap f $ DS.fromList [0.0..100.0]

test__set :: Seq (Vector Double, Double)
test__set = DR.fmap f $ DS.fromList [200.0..250.0]