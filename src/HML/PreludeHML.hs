-- | Definicion de las funciones, clases y tipos comunes en los algoritmos
-- | implementados en la libreria.
module HML.PreludeHML( SupervisedExperiment (SupExp, training_set, test_set,
                                             learning_rate, iterations,
                                             regularization_parameter
                                            ),
                       MultivalueSupExp (MSupExp, training, test, alpha,
                                         max_it ),
                       MLPredictor ((~>)),
                       mse, mseMatrix, plotStats, getTrainingAndTestSets,
                       linearKernel, polyKernel, rbfKernel )
    where

import Control.Parallel
import Control.Parallel.Strategies
import qualified Data.List as DL
import Data.Packed.Vector (Vector)
import qualified Data.Sequence as DS  
import Data.Sequence (Seq,splitAt)
import Prelude hiding (splitAt,zip)
import Data.Foldable
import Graphics.Gnuplot.Simple
import qualified Data.Foldable as DF

{-| @SupervisedExperiment@ Tipo de datos de configuracion de los experimentos de
    aprendizaje supervisado.
  -}
data SupervisedExperiment = SupExp { training_set  :: Seq (Vector Double,Double),
                                     test_set      :: Seq (Vector Double,Double),
                                     learning_rate :: Double,
                                     regularization_parameter :: Double,
                                     iterations    :: Int
                                   }

{-| @MultivalueSupExp@ Tipo de datos de configuracion de los experimentos de
    aprendizaje supervisado multivaluado.
  -}
data MultivalueSupExp = MSupExp { training      :: [ ([Double],[Double]) ],
                                  test          :: [ ([Double],[Double]) ],
                                  alpha         :: Double,
                                  max_it        :: Int
                                }

{-| @MLPredictor@ Maquina que realiza una prediccion a parámetros de entrada.
    La prediccion es multivaluada, en caso de regresar un solo valor este debe
    estar en una lista con ese unico elemento.
  -}
class MLPredictor a where
    (~>)  :: [Double] -> a -> [Double]

jobSize = 1000000

{-| @mse@ Calcula el error cuadratico medio entre dos secuencias. -}
mse :: Seq Double -> Seq Double -> Double
mse a b = (foldl' (\a (x,y) -> a + (x-y)*(x-y)) 0.0 (DS.zip a b)) 
            / ( 2.0 * (fromIntegral $ DS.length a))

parMSE s = do
  case DS.viewl s of
    DS.EmptyL    -> 0.0
    otherwise -> p `par` q `pseq` p+q
  where p = parMSE right
        q = foldl' (\a (x,y) -> a + (x-y)*(x-y)) 0.0 left
        (left,right) = splitAt jobSize s

{-| @mse@ Calcula el error cuadratico medio entre dos matrices
    (listas de listas). -}
mseMatrix :: [[Double]] -> [[Double]] -> Double
mseMatrix a b = (DL.foldl' (+) 0 (zipWith (\x y -> mse (DS.fromList x)
                                                       (DS.fromList y))
                                           a b))
                   / (2.0 * (fromIntegral $ length a))

{-| Muestra las estadísticas de una secuencia de pares de Double. Donde la
    primera coordenada del par define una linea y la segunda coordenada define
    otra. Los valores dados se corresponden con el valor de la imagen, los
    valores del eje horizontal vienen dados por los numeros naturales.
 -}
plotStats :: String                     -- ^ Archivo destino
          -> DS.Seq (Double, Double)    -- ^ Secuencia de valores a graficar
          -> IO()
plotStats pathfile stats = do 
    plotPaths [PNG pathfile, XLabel "Iterations", YLabel "MSE"]
              [tr, ts]
    where tr = toLine $ fmap fst stats
          ts = toLine $ fmap snd stats
          toLine :: DS.Seq Double -> [(Double,Double)]
          toLine s = DL.zip [1.0 .. fromIntegral $ DS.length s] (DF.toList s)

{-| Distribuye los elementos de una lista en dos conjuntos bajo la relación
    1:k, donde k es un parametro de la funcion. Se utiliza para dividir un
    conjunto en los datos de entrenamiento y de pruebas.
 -}
getTrainingAndTestSets :: [a]           -- ^ Lista a dividir
                       -> Int           -- ^ Relacion de particion.
                       -> ([a],[a])     -- ^ Par con las listas resultantes
getTrainingAndTestSets [] _ = ([],[])
getTrainingAndTestSets a k = (ff ++ (fst next), sf ++ (snd next))
    where (f,s)   = DL.splitAt (k+1) a
          (ff,sf) = (take 1 f, take k (tail f))
          next    = getTrainingAndTestSets s k


{-| Genera la misma secuencia de datos pasado como argumento -}
linearKernel   = id

{-| Devuelve la secuencia resultante de elevar la entrada a la k-esima potencia.
 -}
polyKernel 0 x = map (const 1.0) x
polyKernel 1 x = x
polyKernel k x = if k `mod` 2 == 0 then xV polyDiv2 polyDiv2
                                   else xV (polyKernel (k-1) x) x
    where polyDiv2   = polyKernel (k `div` 2) x
          polyMinus1 = polyKernel (k - 1) x
          xV a b = [x*y | x <- a, y <- b]

{-| Devuelve el kernel generado por la funcion de base radial
 -}
rbfKernel gamma a = [ exp( (-gamma)*(x-y)*(x-y) ) | x <- a, y <- a]
