-- | Implementacion de Linear Regression, algoritmo de Machine Learning.
module HML.LinearRegression (LinearRegression(..),
                             linearRegressionGD,
                             linearRegressionGDWithStats,
                             linearRegressionNE,
                             linearRegression)
                             
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

{-|
@g@ funcion de hipotesis para el algoritmo Linear Regression
-}
hypothesis :: Vector Double    -- theta
              -> Vector Double -- x
              -> Double        -- h_theta(x)
hypothesis theta x = theta <.> x  

{-|
@LinearRegression@ tipo de datos para representar 
la maquina de regresion lineal.
-}
data LinearRegression = LinearR (Vector Double)

-- Instancia de la clase MLPredictor para LinearRegression
instance MLPredictor LinearRegression where
  (~>) entry (LinearR theta) =
    [hypothesis (join [scalar 1 :: Vector Double,fromList entry]) theta]

{-|
@logisticRegression@ funcion que calcula los parametros de la funcion de hipotesis
usando el metodo de descendt gradient.
-}
linearRegressionGD :: Double                         -- ^ Tasa de aprendizaje
                      -> Double                      -- ^ Parametro de regularizacion
                      -> Seq (Vector Double, Double) -- ^ Conjunto de entrenamiento (Training set)
                      -> Seq (Vector Double, Double) -- ^ Conjunto de prueba (Test set)
                      -> Int                         -- ^ Numero de caracteristicas provistas para el entrenamiento
                      -> Int                         -- ^ Numero de iteraciones para el calculo
                      -> Vector Double               -- ^ Parametros de la funcion de hipotesis                      
linearRegressionGD alpha lambda tr ts num_features i = do
  let se = SupExp {training_set = tr, test_set = ts, 
                   learning_rate = alpha, regularization_parameter = lambda,
                   iterations = i}
  let initial_theta = randomVector i Gaussian (num_features + 1)
  let (s,_) = execRWS (trainingGD hypothesis costFunction) se (initial_theta,0)
  fst s


{-|
@logisticRegression@ funcion que calcula los parametros de la funcion de hipotesis 
usando el metodo de descendt gradient acarreando y graficando los errores obtenidos, 
durante el calculo, entre los conjuntos de entrenamiento y prueba.
-}
linearRegressionGDWithStats :: Double                         -- learning rate
                               -> Double                      -- regularization parameter
                               -> Seq (Vector Double, Double) -- training set
                               -> Seq (Vector Double, Double) -- test set
                               -> Int                         -- number of features
                               -> Int                         -- max number of iterations
                               -> String
                               -> IO (Vector Double)
linearRegressionGDWithStats alpha lambda tr ts num_features i path = do
  let se = SupExp {training_set = tr, test_set = ts, 
                   learning_rate = alpha, regularization_parameter = lambda,
                   iterations = i}
  let initial_theta = randomVector i Gaussian (num_features + 1)
  let (s,w) = execRWS (trainingGD hypothesis costFunction) se (initial_theta,0)
  plotStats path w
  return $ fst s

{-|
@trainingSet2MatrixVector@ transforma el training set en una matriz cuya primera columna
es de unos y un vector con los y's.
-}
trainingSet2MatrixVector :: Seq (Vector Double, Double)       -- ^ Conjunto de entrenamiento
                            -> (Matrix Double, Vector Double) -- ^ Matriz de disenio y vector y
trainingSet2MatrixVector s = (fromRows m, fromList v)
  where (m,v) = DF.foldl' (\(x',y') (x,y) -> (x'++[x],y'++[y])) ([],[]) s

{-|
@LinearRegressionMonadNE@ transformador monadico usado para el computo del entrenamiento de 
los parametros manteniendo mutable los datos necesarios para el computo y modificando el 
estado de los parametros de la funcion de hipotesis.
-}
type LinearRegressionMonadNE = Reader SupervisedExperiment (Vector Double)

{-|
@trainingNE@ computos destinados a entrenar la maquina de regresio linear bajo el metodo
de normal equation.
-}
trainingNE :: LinearRegressionMonadNE
trainingNE = do
  data_training <- ask
  let trs = DR.fmap one (training_set data_training)
  let lambda = regularization_parameter data_training
  let (x,y) = trainingSet2MatrixVector trs
  let theta = parameters lambda x y
  return theta
    where parameters r m v = (((inv $ trans m `mXm` m) `add` (reg r $ cols m)) `mXm` (trans m)) `mXv` v
          reg a n = diag $ join [(scalar 0 :: Vector Double),constant a $ n - 1] 


{-|
@linearRegressionNE@ funcion que calcula los parametros de la funcion de hipotesis
usando el metodo de normal equation.
-}
linearRegressionNE :: Double                         -- ^ Parametro de regularizacion
                      -> Seq (Vector Double, Double) -- ^ Conjunto de entrenamiento
                      -> Int                         -- ^ Numero de caracteristicas provistas para el entrenamiento
                      -> Vector Double               -- ^ Parametros de la funcion de hipotesis
linearRegressionNE lambda tr num_features = do
  let se = SupExp {training_set = tr, test_set = DS.empty, 
                   learning_rate = 0.0, regularization_parameter = lambda,
                   iterations = 0}
  runReader trainingNE se


{-|
@linearRegressionNE@ funcion que calcula los parametros de la funcion de hipotesis
usando el metodo apropiado. En caso de usarse el metodo del gradiente, se usa la 
tasa de aprendizaje dada.
-}
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

{-|
@costFunction@ funcion de costos de la regresion lineal
-}
costFunction :: Vector Double -- thetas
                -> (Vector Double -> Vector Double -> Double)
                -> Seq (Vector Double, Double)
                -> Double
costFunction th h tr = (1 / toEnum (2 * m)) * (DF.sum $ DR.fmap h' tr) 
  where m = DS.length tr
        h' (x,y) = ((h th x) - y) ^ 2