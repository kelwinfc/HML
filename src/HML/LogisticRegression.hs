module HML.LogisticRegression
where

import HML.Regression
import HML.PreludeHML

import Data.Packed.Vector
import Numeric.Container

import Control.Monad.RWS hiding (join)

import qualified Data.Foldable as DF
import qualified Data.Functor as DR
import qualified Data.Sequence as DS
import Data.Sequence (Seq,empty)


g :: Double -> Double
g z = 1 / (1 + exp (-z))

hypothesis :: Vector Double    -- theta
              -> Vector Double -- x
              -> Double        -- h_theta(x)x
hypothesis theta x = g $ theta <.> x

data LogisticRegression = LogisticR (Vector Double)

instance MLPredictor LogisticRegression where
  (~>) entry (LogisticR theta) = 
    if h >= 0.5 
    then [1]
    else [0]
      where h = hypothesis (join [scalar 1 :: Vector Double,fromList entry]) theta

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
  let initial_theta = randomVector i Gaussian (num_features + 1)
  let (s,_) = execRWS (trainingGD hypothesis costFunction)  se (initial_theta,0)
  fst s
  
logisticRegressionWithStats :: Double 
                               -> Double 
                               -> Seq (Vector Double, Double) 
                               -> Int 
                               -> Int
                               -> String
                               -> IO (Vector Double)
logisticRegressionWithStats alpha lambda tr num_features i path = do
  let se = SupExp {training_set = tr, test_set = empty, 
                   learning_rate = alpha, regularization_parameter = lambda,
                   iterations = i}
  let initial_theta = randomVector i Gaussian (num_features + 1)
  let (s,w) = execRWS (trainingGD hypothesis costFunction)  se (initial_theta,0)
  plotStats path w
  return $ fst s
  
costFunction :: Vector Double -- thetas
                -> (Vector Double -> Vector Double -> Double)
                -> Seq (Vector Double, Double)
                -> Double
costFunction th h tr = (1 / toEnum (m)) * (DF.sum $ DR.fmap cost tr)
  where m = DS.length tr
        cost (x,y) = (-y * log(h th x)) - ((1 - y) * log(1 - (h th x)))