module HML.Regression where

import Data.Packed.Vector
import Numeric.LinearAlgebra
import Numeric.GSL.Minimization

import qualified Data.Foldable as DF
import qualified Data.Functor as DR

import Control.Parallel
import Control.Parallel.Strategies

import Data.Sequence (Seq)
import qualified Data.Sequence as DS
import qualified Data.List as DL

import Control.Monad.RWS hiding (join)

import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import HML.PreludeHML

type RegressionMonadGD = RWS SupervisedExperiment (Seq(Double,Double)) (Vector Double) () 

trainingGD :: (Vector Double -> Vector Double -> Double) 
            -> (Vector Double 
                -> (Vector Double -> Vector Double -> Double) 
                -> Seq (Vector Double, Double) 
                -> Double)
            -> RegressionMonadGD
trainingGD h c = do
  data_training <- ask
  theta <- get
  let it = iterations data_training
  let alpha  = learning_rate data_training
  let lambda = regularization_parameter data_training
  let trs    = DR.fmap one (training_set data_training)
  let tss    = DR.fmap one (test_set data_training)  
  let s = dim theta  
  let (new_theta,_) = minimizeV NMSimplex2 (10 ** (-3)) it (constant 1 s)
                          (\th -> c th h trs) theta
  put new_theta
          
one :: (Vector Double, Double) 
       -> (Vector Double, Double)
one (x,y) = (join [(scalar 1 :: Vector Double),x],y)