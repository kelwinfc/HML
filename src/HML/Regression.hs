module HML.Regression where

import Data.Packed.Vector

import qualified Data.Foldable as DF
import qualified Data.Functor as DR

import Control.Parallel
import Control.Parallel.Strategies

import Data.Sequence (Seq)
import qualified Data.Sequence as DS

import Control.Monad.RWS hiding (join)

import HML.PreludeHML

type RegressionMonadGD = RWS SupervisedExperiment (Seq (Double,Double)) (Vector Double,Int) ()

trainingGD :: (Vector Double -> Vector Double -> Double) 
              -> RegressionMonadGD
trainingGD h = do
  data_training <- ask
  (theta,i) <- get
  let it = iterations data_training
  if it /= 0 && i < it 
    then do 
      let trs = DR.fmap one (training_set data_training)
      let tss = DR.fmap one (test_set data_training)
      let alpha = learning_rate data_training
      let lambda = regularization_parameter data_training
      
      -- Calculo de la nueva funcion de hipotesis
      let theta_0 = calculate_theta h (theta @> 0) alpha 0.0 trs theta 0
      let parameters =  calculate_parameters h alpha lambda trs theta 1
      let new_theta = join [fromList[theta_0],parameters]
      
      -- Calculo del error
      let trs_get = DR.fmap (h' new_theta) trs
      let tss_get = DR.fmap (h' new_theta) tss
      let h_trs = DR.fmap fst trs_get
      let y_trs = DR.fmap snd trs_get
      let h_tss = DR.fmap fst tss_get
      let y_tss = DR.fmap snd tss_get
      tell $ DS.singleton (mse h_trs y_trs,mse h_tss y_tss)
      put (new_theta, i + 1)
      trainingGD h
    else return ()
  where h' th (x,y) = (h th x,y)

one :: (Vector Double, Double) 
       -> (Vector Double, Double)
one (x,y) = (join [fromList [1],x],y)

derivedJtheta :: (Vector Double -> Vector Double -> Double) -- hypothesis function
                 -> Seq (Vector Double,Double)              -- training set
                 -> Vector Double                           -- theta
                 -> Double                                  -- lambda
                 -> Int                                     -- index j to calculate
                 -> Double                                  -- value of derived cost function respect to theta_j 
derivedJtheta h tr th lambda j =  ((DF.sum $ DR.fmap (e th j) tr) + lambda * (th @> j))/ (toEnum m)
   where m = DS.length tr
         e t j (x,y) = ((h t x) - y) * (x @> j)

calculate_theta :: (Vector Double -> Vector Double -> Double) -- hypothesis function
                   -> Double                     -- previous theta value
                   -> Double                     -- alpha                                                
                   -> Double                     -- lambda
                   -> Seq (Vector Double,Double) -- training set
                   -> Vector Double              -- previous theta set
                   -> Int                        -- index j to calculate
                   -> Double                     -- new theta
calculate_theta h th_pr alpha lambda tr th j = 
  th_pr - (alpha * (derivedJtheta h tr th lambda j))
  
calculate_parameters :: (Vector Double -> Vector Double -> Double) -- hypothesis function
                        -> Double                     -- alpha
                        -> Double                     -- lambda
                        -> Seq (Vector Double,Double) -- training set
                        -> Vector Double              -- previous theta set
                        -> Int                        -- initial j
                        -> Vector Double              -- new theta set
calculate_parameters h alpha lambda tr th_pr j = 
  if j == n 
  then (fromList[calculate_theta h  (th_pr @> j) alpha lambda tr th_pr j])
  else (th_j `par` sub_th `pseq` join [fromList[th_j],sub_th])
  where  n      = dim th_pr - 1
         th_j   = calculate_theta h (th_pr @> j) alpha lambda tr th_pr j
         sub_th = (calculate_parameters h alpha lambda tr th_pr (j + 1))
