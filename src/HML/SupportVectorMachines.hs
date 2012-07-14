module HML.SupportVectorMachines where

import Data.Sequence (Seq)
import qualified Data.Sequence as DS

import Numeric.GSL.Minimization

import qualified Data.Foldable as DF
import HML.PreludeHML

data SupportVectorMachine = SVM ([Double] -> [Double]) -- Kernel function
                                [Double]               -- Hypothesis

instance MLPredictor SupportVectorMachine where
    (~>) entries (SVM kernel weights) = 
            [ sigmoid (sum $ zipWith (*) (1:(kernel entries)) weights ) ]
            where sigmoid x = 1.0 / (1.0 + exp(-x))

cost_function :: Double -> Seq ([Double],Double) -> [Double] -> Double
cost_function reg set theta = sum_cost
            + (reg/2.0) * (DF.sum $ fmap (**2.0) (tail theta))
    where sum_cost = 
            DF.foldl' (\ac (x,y) -> ac - (y * (myLog $ h theta x))
                                     - ((1-y)*(myLog $ 1-(h theta x)) )
                      )
                0.0 set
          h t x = 1.0 / (1.0 + exp (- (sum $ zipWith (*) t (1:x))))
          myLog x = if x < 1e-6 then log 1e-6 else log x

train :: ([Double] -> [Double])   -- Kernel Function
      -> Int                      -- Iterations
      -> Double                   -- Regularization Parameter
      -> Seq ([Double], [Double]) -- Training Set
      -> Seq ([Double], [Double]) -- Test Set
      -> SupportVectorMachine

train kernel iterations reg tr ts = SVM kernel weights
    where weights = fst $ minimize NMSimplex2 0.0 iterations [1,1]
                                   ( \x -> cost_function reg tr' x )
                                   (replicate num_features 1)
          num_features = 1 + (length $ fst $ DS.index tr 0)
          tr' = fmap (\(x,y) -> (kernel x, head y)) tr
