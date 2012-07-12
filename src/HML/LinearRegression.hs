module LinearRegression(linearRegressionGD,linearRegressionNE,(~>),linearRegression) where

import Control.Monad.Reader hiding (join)
import Control.Monad.RWS hiding (join)

import Control.Parallel
import Control.Parallel.Strategies

import Data.Sequence (Seq)
import qualified Data.Sequence as DS

import Data.Packed.Vector
import Data.Packed.Matrix
import Numeric.LinearAlgebra.Algorithms

import Numeric.Container 

import qualified Data.Foldable as DF
import qualified Data.Functor as DR

import Graphics.Gnuplot.Simple

import PreludeHML

(~>) :: Vector Double    -- theta
              -> Vector Double -- x
              -> Double        -- h_theta(x)
(~>) theta x = theta <.> x

derivedJtheta :: (Vector Double -> Vector Double -> Double) -- hypothesis function
                 -> Seq (Vector Double,Double)              -- training set
                 -> Vector Double                           -- theta
                 -> Double                                  -- lambda
                 -> Int                                     -- index j to calculate
                 -> Double                                  -- value of derived cost function respect to theta_j 
derivedJtheta h tr th lambda j =  ((DF.sum $ DR.fmap (e th j) tr) + lambda * (th @> j))/ (toEnum m)
   where m = DS.length tr
         e t j (x,y) = ((h t x) - y) * (x @> j)

calculate_theta :: Double                        -- previous theta value
                   -> Double                     -- alpha                                                
                   -> Double                     -- lambda
                   -> Seq (Vector Double,Double) -- training set
                   -> Vector Double              -- previous theta set
                   -> Int                        -- index j to calculate
                   -> Double                     -- new theta
calculate_theta th_pr alpha lambda tr th j = 
  th_pr - (alpha * (derivedJtheta (~>) tr th lambda j))
  
calculate_parameters :: Double                        -- alpha
                        -> Double                     -- lambda
                        -> Seq (Vector Double,Double) -- training set
                        -> Vector Double              -- previous theta set
                        -> Int                        -- initial j
                        -> Vector Double              -- new theta set
calculate_parameters alpha lambda tr th_pr j = 
  if j == n 
  then (fromList[calculate_theta  (th_pr @> j) alpha lambda tr th_pr j])
  else (th_j `par` sub_th `pseq` join [fromList[th_j],sub_th])
  where  n      = dim th_pr - 1
         th_j   = calculate_theta (th_pr @> j) alpha lambda tr th_pr j
         sub_th = (calculate_parameters alpha lambda tr th_pr (j + 1))

type LinearRegressionMonadGD = RWS SupervisedExperiment (Seq (Double,Double)) (Vector Double,Int) ()

trainingGD :: LinearRegressionMonadGD
trainingGD = do
  data_training <- ask
  (theta,i) <- get
  let it = iterations data_training
  if it /= 0 && i < it 
    then do 
      let trs = DR.fmap one (training_set data_training)
      let tss = DR.fmap one (test_set data_training)
      let alpha = learning_rate data_training
      let lambda = regularization_parameter data_training
      let theta_0 = calculate_theta (theta @> 0) alpha 0.0 trs theta 0
      let parameters =  calculate_parameters alpha lambda trs theta 1
      let new_theta = join [fromList[theta_0],parameters]
      let trs_get = DR.fmap (h new_theta) trs
      let tss_get = DR.fmap (h new_theta) tss
      let h_trs = DR.fmap fst trs_get
      let y_trs = DR.fmap snd trs_get
      let h_tss = DR.fmap fst tss_get
      let y_tss = DR.fmap snd tss_get
      tell $ DS.singleton (mse h_trs y_trs,mse h_tss y_tss)
      put (new_theta, i + 1)
      trainingGD
    else return ()
  where h th (x,y) = (th ~> x,y)

one :: (Vector Double, Double) 
       -> (Vector Double, Double)
one (x,y) = (join [fromList [1],x],y)

linearRegressionGD :: Double                         -- learning rate
                      -> Double                      -- regularization parameter
                      -> Seq (Vector Double, Double) -- training set
                      -> Seq (Vector Double, Double) -- test set
                      -> Int                         -- number of features
                      -> Int                         -- max number of iterations
                      -> Vector Double
linearRegressionGD alpha lambda tr ts num_features i = do
  let se = SupExp {training_set = tr, test_set = ts, 
                   learning_rate = alpha, regularization_parameter = lambda,
                   iterations = i}
  let initial_theta = constant 1 (num_features + 1)
  let (s,w) = execRWS trainingGD se (initial_theta,0)
  fst s

trainingSet2MatrixVector :: Seq (Vector Double, Double)       -- training set
                            -> (Matrix Double, Vector Double) -- desing matrix and y
trainingSet2MatrixVector s = (fromRows m, fromList v)
  where (m,v) = DF.foldl' (\(x',y') (x,y) -> (x'++[x],y'++[y])) ([],[]) s

type LinearRegressionMonadNE = Reader SupervisedExperiment (Vector Double)

trainingNE :: LinearRegressionMonadNE
trainingNE = do
  data_training <- ask
  let trs = DR.fmap one (training_set data_training)
  let lambda = regularization_parameter data_training
  let (x,y) = trainingSet2MatrixVector trs
  let theta = parameters lambda x y
  return theta
    where parameters r m v = (((inv $ trans m `mXm` m) `add` (reg r $ cols m)) `mXm` (trans m)) `mXv` v
          reg a n = diag $ join [fromList [0],constant a $ n - 1] 

linearRegressionNE :: Double                         -- regularization parameter
                      -> Seq (Vector Double, Double) -- training set
                      -> Seq (Vector Double, Double) -- test set
                      -> Int                         -- number of features
                      -> Vector Double
linearRegressionNE lambda tr ts num_features = do
  let se = SupExp {training_set = tr, test_set = ts, 
                   learning_rate = 0.0, regularization_parameter = lambda,
                   iterations = 0}
  runReader trainingNE se

linearRegression :: Double                          -- learning rate
                    -> Double                       -- regularization parameter
                    -> Seq (Vector Double, Double)  -- training set
                    -> Seq (Vector Double, Double)  -- test set
                    -> Int                          -- number of features
                    -> Int                          -- iterations
                    -> Vector Double
linearRegression alpha lambda tr ts f i = if f < 1000 
                                        then (linearRegressionGD alpha lambda tr ts f i)
                                        else (linearRegressionNE lambda tr ts f)

---------------- EXAMPLE 

f x = (fromList [x],-2+3*x)
g x = (fromList [x],3+2*x)

(<<) x y = if x < y then (fromList[x,y],1.0) else (fromList[x,y],0.0)

prueba_tr :: Seq (Vector Double, Double)
prueba_tr = DR.fmap g $ DS.fromList [0.0,2.0,3.0,5.0]

prueba_ts :: Seq (Vector Double, Double)
prueba_ts = DR.fmap g $ DS.fromList [10.0,12.0,13.0,15.0]

training__set :: Seq (Vector Double, Double)
training__set = DR.fmap f $ DS.fromList [0.0..100.0]

test__set :: Seq (Vector Double, Double)
test__set = DR.fmap f $ DS.fromList [200.0..250.0]