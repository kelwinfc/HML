-- | Implementacion de Logistic Regression, algoritmo de Machine Learning.
module HML.LogisticRegression (LogisticRegression(..),
                               logisticRegression,
                               logisticRegressionWithStats)
                               
                               
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


{-|
@g@ funcion de hipotesis para el algoritmo Logistic Regression
-}
hypothesis :: Vector Double    -- theta
              -> Vector Double -- x
              -> Double        -- h_theta(x)x
hypothesis theta x = g $ theta <.> x
  where g :: Double -> Double
        g z = 1 / (1 + exp (-z))

{-|
@LogisticRegression@ tipo de datos para representar 
la maquina de regresion logistica.
-}
data LogisticRegression = LogisticR (Vector Double)

-- Instancia de la clase MLPredictor para LogisticRegression
instance MLPredictor LogisticRegression where
  (~>) entry (LogisticR theta) = 
    if h >= 0.5 
    then [1]
    else [0]
      where h = hypothesis (join [scalar 1 :: Vector Double,fromList entry]) theta

{-|
@logisticRegression@ funcion que calcula los parametros de la funcion de hipotesis
-}
logisticRegression :: Double                         -- ^ Tasa de aprendizaje
                      -> Double                      -- ^ Parametro de regularizacion
                      -> Seq (Vector Double, Double) -- ^ Conjunto de entrenamiento (Training set)
                      -> Int                         -- ^ Numero de caracteristicas provistas para el entrenamiento
                      -> Int                         -- ^ Numero de iteraciones para el calculo
                      -> Vector Double               -- ^ Parametros de la funcion de hipotesis
logisticRegression alpha lambda tr num_features i = do
  let se = SupExp {training_set = tr, test_set = empty, 
                   learning_rate = alpha, regularization_parameter = lambda,
                   iterations = i}
  let initial_theta = randomVector i Gaussian (num_features + 1)
  let (s,_) = execRWS (trainingGD hypothesis costFunction)  se (initial_theta,0)
  fst s
  
{-|  
@logisticRegressionWithStats@ funcion que calcula los parametros de la funcion de hipotesis
acarreando y graficando los errores obtenidos, durante el calculo, entre los conjuntos de 
entrenamiento y prueba.
-}
logisticRegressionWithStats :: Double                         -- ^ Tasa de aprendizaje
                               -> Double                      -- ^ Parametro de regularizacion
                               -> Seq (Vector Double, Double) -- ^ Conjunto de entrenamiento (Training set)
                               -> Seq (Vector Double, Double) -- ^ Conjunto de prueba (Test set)
                               -> Int                         -- ^ Numero de iteraciones para el calculo 
                               -> Int                         -- ^ Numero de caracteristicas provistas para el entrenamiento
                               -> String                      -- ^ Nombre del archivo para la grafica
                               -> IO (Vector Double)          -- ^ Parametros de la funcion de hipotesis
logisticRegressionWithStats alpha lambda tr ts num_features i path = do
  let se = SupExp {training_set = tr, test_set = ts, 
                   learning_rate = alpha, regularization_parameter = lambda,
                   iterations = i}
  let initial_theta = randomVector i Gaussian (num_features + 1)
  let (s,w) = execRWS (trainingGD hypothesis costFunction)  se (initial_theta,0)
  plotStats path w
  return $ fst s

{-|
@costFunction@ funcion de costos de la regresion logistica
-}
costFunction :: Vector Double -- thetas
                -> (Vector Double -> Vector Double -> Double)
                -> Seq (Vector Double, Double)
                -> Double
costFunction th h tr = (1 / toEnum (m)) * (DF.sum $ DR.fmap cost tr)
  where m = DS.length tr
        cost (x,y) = (-y * log(h th x)) - ((1 - y) * log(1 - (h th x)))