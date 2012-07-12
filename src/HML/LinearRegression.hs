module HML.LinearRegression (linearRegression,
                             linearRegressionGD,
                             linearRegressionGDWithStats,
                             linearRegressionNE,
                             (~>),(<~),
                             trainingSet2MatrixVector,
                             hypothesis)
where

import Control.Monad.Reader hiding (join)
import Control.Monad.RWS hiding (join)

import Control.Parallel
import Control.Parallel.Strategies

import Data.Sequence (Seq)
import qualified Data.Sequence as DS

import Data.Packed.Vector
import Data.Packed.Matrix
import Numeric.Container
import Numeric.LinearAlgebra.Algorithms

import qualified Data.Foldable as DF
import qualified Data.Functor as DR

import Graphics.Gnuplot.Simple

import HML.PreludeHML
import HML.Regression

hypothesis :: Vector Double    -- theta
              -> Vector Double -- x
              -> Double        -- h_theta(x)
hypothesis theta x = theta <.> x

(~>) :: Vector Double -> Vector Double -> Double
(~>) values training_parameters = 
  hypothesis (join [fromList[1],values]) training_parameters

(<~) :: Vector Double -> Vector Double -> Double
(<~) training_parameters values = values ~> training_parameters

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
  let initial_theta = constant 0 (num_features + 1)
  let (s,_) = execRWS (trainingGD hypothesis costFunction) se (initial_theta,0)
  fst s

linearRegressionGDWithStats :: Double                         -- learning rate
                               -> Double                      -- regularization parameter
                               -> Seq (Vector Double, Double) -- training set
                               -> Seq (Vector Double, Double) -- test set
                               -> Int                         -- number of features
                               -> Int                         -- max number of iterations
                               -> IO (Vector Double)
linearRegressionGDWithStats alpha lambda tr ts num_features i = do
  let se = SupExp {training_set = tr, test_set = ts, 
                   learning_rate = alpha, regularization_parameter = lambda,
                   iterations = i}
  let initial_theta = constant 0 (num_features + 1)
  let (s,w) = execRWS (trainingGD hypothesis costFunction) se (initial_theta,0)
  plotStats "Graphics Errors of Linear Regression.png" w
  return $ fst s

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
                      -> Int                         -- number of features
                      -> Vector Double
linearRegressionNE lambda tr num_features = do
  let se = SupExp {training_set = tr, test_set = DS.empty, 
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
                                        else (linearRegressionNE lambda tr f)

costFunction :: Vector Double -- thetas
                -> (Vector Double -> Vector Double -> Double)
                -> Seq (Vector Double, Double)
                -> Double
costFunction th h tr = (1 / toEnum (2 * m)) * (DF.sum $ DR.fmap h' tr) 
  where m = DS.length tr
        h' (x,y) = ((h th x) - y) ^ 2
