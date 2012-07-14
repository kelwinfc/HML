-- | Implementacion de las funciones y tipos necesarios para la aplicacion de los algoritmos de regresion

module HML.Regression (trainingGD,
                       RegressionMonadGD,
                       one)
       where

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

{-| 
@RegressionMonadGD@ transformador monadico usado para el computo de los metodos de regresion para el entrenamiento de
maquinas manteniendo sus datos dados, acarreando errores para posible graficacion y cambiando el estado de los 
parametros e iteraciones.
-}
type RegressionMonadGD = RWS SupervisedExperiment (Seq (Double,Double)) (Vector Double,Int) ()

trainingGD :: (Vector Double 
               -> Vector Double 
               -> Double              )      -- ^ funcion de hipotesis
            -> (Vector Double                              
                -> (Vector Double 
                    -> Vector Double 
                    -> Double)               
                -> Seq (Vector Double, Double) 
                -> Double)                   -- ^ funcion de costos
            -> RegressionMonadGD
trainingGD h c = do
  data_training <- ask
  (theta,i) <- get
  let it = iterations data_training
  if it /= 0 && i < it 
    then do
      let alpha  = learning_rate data_training
      let lambda = regularization_parameter data_training
      let trs    = DR.fmap one (training_set data_training)
      let tss    = DR.fmap one (test_set data_training)  
      let s = dim theta  
      let (new_theta,_) = minimizeV NMSimplex2 (10 ** (-3)) 1 (constant 1 s)
                          (\th -> c th h trs) theta
      let (h_trs,y_trs) = unzipSeq $ DR.fmap (h' new_theta) trs
      let (h_tss,y_tss) = unzipSeq $ DR.fmap (h' new_theta) tss
      tell $ DS.singleton $ (mse h_trs y_trs, mse h_tss y_tss)
      put (new_theta,i + 1)
      trainingGD h c
    else 
      return ()
  where h' t (x,y) = (h x t,y)
        unzipSeq s = (DR.fmap fst s, DR.fmap snd s)
          
{-|
@one@ funcion para aniadir la coordenada 1 a los vectores del training set.
-}
one :: (Vector Double, Double) 
       -> (Vector Double, Double)
one (x,y) = (join [(scalar 1 :: Vector Double),x],y)