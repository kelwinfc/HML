-- | Implementacion de maquinas de soporte de vectores.

module HML.SupportVectorMachines (SupportVectorMachine (SVM), train)
    where

import Data.Sequence (Seq)
import qualified Data.Sequence as DS

import Numeric.GSL.Minimization

import qualified Data.Foldable as DF
import HML.PreludeHML

{-| Definicion del tipo @SupportVectorMachine@ que representa las maquinas
    de este modulo. El primer argumento se corresponde con la funcion de
    kernel y el segundo con la funcion de hipotesis. -}
data SupportVectorMachine = SVM ([Double] -> [Double])
                                [Double]              

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

{- | Entrena una maquina de soporte de vectores utilizando la funcion simplex.
 -}
train :: ([Double] -> [Double])   -- ^ Kernel
      -> Int                      -- ^ Maximo numero de iteraciones
      -> Double                   -- ^ Parametro de regularizacion
      -> Seq ([Double], [Double]) -- ^ Conjunto de entrenamiento
      -> SupportVectorMachine     -- ^ SVM
train kernel iterations reg tr = SVM kernel weights
    where weights = fst $ minimize NMSimplex2 0.0 iterations
                                   (replicate num_features 100.0)
                                   ( \x -> cost_function reg tr' x )
                                   (replicate num_features 1)
          num_features = 1 + (length $ fst $ DS.index tr 0)
          tr' = fmap (\(x,y) -> (kernel x, head y)) tr
