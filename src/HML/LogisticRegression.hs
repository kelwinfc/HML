module HML.LogisticRegression (logisticRegression,
                               logisticRegressionWithStats,
                               (~>),(<~)) 
where

import HML.Regression
import HML.PreludeHML

import Data.Packed.Vector
import Numeric.Container

import Control.Monad.RWS hiding (join)

import Data.Sequence (Seq,empty)

g :: Double -> Double
g z = 1 / (1 + exp ((-1) * z))

hypothesis :: Vector Double    -- theta
              -> Vector Double -- x
              -> Double        -- h_theta(x)
hypothesis theta x = g $ theta <.> x

(~>) :: Vector Double -> Vector Double -> Int
(~>) values training_parameters = if h >= 0.5 
                                  then 1
                                  else 0
                                                   
        where h = hypothesis (join [fromList[1],values]) training_parameters

(<~) :: Vector Double -> Vector Double -> Int
(<~) training_parameters values = values ~> training_parameters

logisticRegression :: Double 
                      -> Double 
                      -> Seq (Vector Double, Double) 
                      -> Int 
                      -> Int
                      -> Vector Double
logisticRegression alpha lambda tr num_features i = do
  let se = SupExp {training_set = tr, test_set = empty, 
                   learning_rate = alpha, regularization_parameter = lambda,
                   iterations = i}
  let initial_theta = constant 0 (num_features + 1)
  let (s,_) = execRWS (trainingGD hypothesis) se (initial_theta,0)
  fst s
  

logisticRegressionWithStats :: Double 
                               -> Double 
                               -> Seq (Vector Double, Double) 
                               -> Seq (Vector Double, Double)                                                      
                               -> Int 
                               -> Int
                               -> IO(Vector Double)
logisticRegressionWithStats alpha lambda tr ts num_features i = do
  let se = SupExp {training_set = tr, test_set = empty, 
                   learning_rate = alpha, regularization_parameter = lambda,
                   iterations = i}
  let initial_theta = constant 0 (num_features + 1)
  let (s,w) = execRWS (trainingGD hypothesis) se (initial_theta,0)
  plotStats "Graphics Errors of Logistic Regression.png" w
  return $ fst s
