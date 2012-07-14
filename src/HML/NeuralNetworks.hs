-- | Implementacion de una red neural feedward con una capa oculta, entrenada
-- | con el algoritmo de backpropagation estocastico paralelo.
module HML.NeuralNetworks ( NeuralNetwork (ANN), LinearUnit (LinearU),
                            StepUnit (StepU), SigmoidUnit (SigmoidU),
                            createNeuralNetwork,
                            backpropagation )
    where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import HML.PreludeHML
import Data.List

import Control.Parallel
import Control.Parallel.Strategies

import Control.Monad.Reader hiding (join)
import Control.Monad.RWS hiding (join)
import Data.Sequence (Seq)
import qualified Data.Sequence as DS

{-| Combinador monadico utilizado en el aprendizaje del algoritmo de
    backpropagation -}
type BackPropagationExp = 
    RWS MultivalueSupExp (Seq (Double,Double)) (NeuralNetwork, Int) ()

{-| Neurona con operador lineal -}    
data LinearUnit = LinearU [Double]
    deriving Show

instance MLPredictor LinearUnit where
    (~>) entries (LinearU weights) = [neuralDot (1:entries) weights]

{-| Neurona con operador de salto -}
data StepUnit = StepU Double [Double]
    deriving Show

instance MLPredictor StepUnit where
    (~>) entries (StepU theta weights) = 
            if (neuralDot (1:entries) weights) >= theta then [1.0]
                                                        else [0.0]

{-| Neurona con operador sigmoide -}
data SigmoidUnit = SigmoidU [Double]
    deriving Show

instance MLPredictor SigmoidUnit where
    (~>) entries (SigmoidU weights) =
        [ 1.0 / ( 1.0 + (exp (- (neuralDot entries weights) ))) ]

{-| Definicion del tipo de datos que representa la red neural. El primer entero
    se corresponde con el numero de entradas y el segundo, con la lista de
    neuronas en cada una de las capas internas y la de salida. -}
data NeuralNetwork = ANN Int
                         [[SigmoidUnit]]
    deriving Show

instance NFData NeuralNetwork

instance MLPredictor NeuralNetwork where
    (~>) a (ANN nentries hidden) = if length a == nentries
                                      then foldl' outputLayer a hidden
                                      else error "Numero de entradas incorrecto"


{-| @createNeuralNetwork@ Crea una red neural con pesos iniciales aleatorios
    con la topologia indicada -}
createNeuralNetwork :: [Int] -> IO ( NeuralNetwork )
createNeuralNetwork (x:xs) = createNeuralNetwork' x xs

createNeuralNetwork' i [] = do 
    return (ANN i [])
createNeuralNetwork' i (x:xs) = do
        bigList <- ( sample' $ vectorOf x $ vectorOf (i+1) $
                         choose (-0.5,0.5) )::IO [[[Double]]]
        let units = map (\a -> SigmoidU a) (head bigList)
        (ANN _ sub) <- createNeuralNetwork' x xs
        return $ ANN i (units:sub)

neuralDot a b = if length a + 1 == length b 
                   then sum $ zipWith (*) (1:a) b
                   else error "Numero incorrecto de argumentos"

outputLayer :: MLPredictor a => [Double] -> [a] -> [Double]
outputLayer entries = map (\x -> head $ entries ~> x)

addANN :: NeuralNetwork -> NeuralNetwork -> NeuralNetwork
addANN (ANN i w0) (ANN _ w1) = ANN i $ (zipWith (zipWith sumUnits) w0 w1)
    where sumUnits (SigmoidU u0) (SigmoidU u1) = SigmoidU (zipWith (+) u0 u1)

subANN :: NeuralNetwork -> NeuralNetwork -> NeuralNetwork
subANN (ANN i w0) (ANN _ w1) = ANN i $ (zipWith (zipWith sumUnits) w0 w1)
    where sumUnits (SigmoidU u0) (SigmoidU u1) = SigmoidU (zipWith (-) u0 u1)

splitInThunks :: Int       -- ^ Tamaño de cada grupo
              -> [a]       -- ^ Secuencia de datos
              -> [[a]]     -- ^ Grupos de datos
splitInThunks n l = unfoldr go l
    where go xs = if null $ fst $ sp xs
                     then Nothing
                     else Just (sp xs)
          sp xs = splitAt n xs

annJobSize = 1000

backprop :: BackPropagationExp
backprop = do
  config <- ask
  (nn,i) <- get
  let it = max_it config
  if it /= 0 && i < it 
    then do
        -- Datos de prueba y entrenamiento
        let training_s   = training config
        let test_s       = test config
        
        -- Ajuste de pesos
        let training_thunks = splitInThunks annJobSize training_s
        let new_nns = paralelDiffs (alpha config) nn training_thunks
        let new_nn = foldl' addANN nn new_nns
        
        -- Generacion de estadisticas de errores
        let (out_tr, out_ts) = getOuts new_nn training_s test_s
        let e@(err_tr, _) = getStats out_tr out_ts training_s test_s
        tell $ DS.singleton e
        
        put $ (new_nn, i + 1)
        backprop
    else
        return ()
    where paralelDiffs a nn [] = []
          paralelDiffs a nn (x:xs) = p `par` q `pseq` p:q
                where p = (subANN (joinANN a nn x) nn) `using` rdeepseq
                      q = paralelDiffs a nn xs `using` rdeepseq
                      
          joinANN a orig_nn tr = foldl' (\prev_n t -> backProp' a prev_n t)
                                         orig_nn tr
           
           -- Calculo de las estadisticas en paralelo para el conjunto de
           -- entrenamiento y de pruebas
          getOuts new_nn training_s test_s = p `par` q `pseq` (p,q)
              where p = map (\x -> x ~> new_nn) (map fst training_s) `using` rdeepseq
                    q = map (\x -> x ~> new_nn) (map fst test_s) `using` rdeepseq
          getStats out_tr out_ts training_s test_s = p `par` q `pseq` (p,q)
              where p = mseMatrix out_tr (map snd training_s) `using` rdeepseq
                    q = mseMatrix out_ts (map snd test_s) `using` rdeepseq
                             
backProp' :: Double -> NeuralNetwork -> ([Double],[Double]) -> NeuralNetwork
backProp' alpha nn@(ANN i nss) (xs,ys) = 
            ANN i [aux (head nss) ds_hidden (1:xs),
                   aux (nss !! 1) ds_out (1:out_hidden)]
    where out_hidden = outputLayer xs (head nss)
          out_out    = outputLayer out_hidden (nss !! 1)
          ds_out = zipWith (\s y -> s * (1-s)*(y-s)) out_out ys
          ds_hidden = zipWith (\x s -> x * (1-x) * s) out_hidden 
                          $ map (sum . zipWith (*) ds_out) . 
                                              transpose $ weights !! 1
          weights = map (map (\(SigmoidU w) -> w)) nss
          
          aux :: [SigmoidUnit] -> [Double] -> [Double] -> [SigmoidUnit]
          aux l delta_l entry_l = zipWith (\(SigmoidU x) y -> 
                                                  SigmoidU (zipWith (+) x y))
                                        l
                                        (
                                        map (\(w,d) -> map (*(d*alpha)) w) $ 
                                            zip (replicate 
                                               (length delta_l) entry_l) delta_l
                                        )

backpropagation :: String                 -- ^ Archivo de salida de estadisticas
                -> Double                 -- ^ Tasa de aprendizaje
                -> [([Double],[Double])]  -- ^ Conjunto de Entrenamiento
                -> [([Double],[Double])]  -- ^ Conjunto de Pruebas
                -> Int                    -- ^ Numero de Iteraciones
                -> [Int]                  -- ^ Topologia
                -> IO (NeuralNetwork)     -- ^ Red Neural entrenada
backpropagation plot_name a tr ts i topology = do
  if ( length topology /= 3 )
     then error "La red debe tener una capa de entrada, una de salida y una oculta"
     else do let se = MSupExp { training = tr, test = ts, alpha = a,
                                max_it = i}
             initial_nn <- createNeuralNetwork topology
             let (_,(s,_),w) = runRWS backprop se (initial_nn,0)
             plotStats plot_name w
             return s
