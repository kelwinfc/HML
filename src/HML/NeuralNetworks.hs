module HML.NeuralNetworks
    where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import PreludeHML
import Data.List

import Control.Monad.Reader hiding (join)
import Control.Monad.RWS hiding (join)
import Data.Sequence (Seq)
import qualified Data.Sequence as DS

type BackPropagationExp = 
    RWS MultivalueSupExp (Seq (Double,Double)) (NeuralNetwork, Int) ()

class Neuron a where
    
    (~>) :: [Double] -> a -> Double
    (~>) entries neuron = neuron <~ entries
    
    (<~) :: a -> [Double] -> Double
    (<~) neuron entries = entries ~> neuron

data LinearUnit = LinearU [Double]
    deriving Show

instance Neuron LinearUnit where
    (~>) entries (LinearU weights) = neuralDot entries weights

data StepUnit = StepU Double [Double]
    deriving Show

instance Neuron StepUnit where
    (~>) entries (StepU theta weights) = 
            if (neuralDot entries weights) >= theta then 1.0
                                                    else 0.0

data SigmoidUnit = SigmoidU [Double]
    deriving Show

instance Neuron SigmoidUnit where
    (~>) entries (SigmoidU weights) =
        1.0 / ( 1.0 + (exp (- (neuralDot entries weights) )))

data NeuralNetwork = ANN Int [[SigmoidUnit]]
    deriving Show

--createNeuralNetork :: [Int] -> NeuralNetwork
createNeuralNetwork (x:xs) = createNeuralNetwork' x xs

createNeuralNetwork' i [] = do 
    return (ANN i [])
createNeuralNetwork' i (x:xs) = do
        bigList <- ( sample' $ vectorOf x $ vectorOf (i+1) $
                         choose (-0.1,0.2) )::IO [[[Double]]]
        let units = map (\a -> SigmoidU a) (head bigList)
        (ANN _ sub) <- createNeuralNetwork' x xs
        return $ ANN i (units:sub)

neuralDot a b = if length a + 1 == length b then sum $ zipWith (*) (1:a) b
                                            else error "Numero incorrecto de argumentos"

outputLayer :: Neuron a => [Double] -> [a] -> [Double]
outputLayer entries = map (entries ~>)

(~~>) :: [Double] -> NeuralNetwork -> [Double]
(~~>) a (ANN nentries hidden) = foldl' outputLayer
                                       a
                                       hidden

outputs :: [Double] -> NeuralNetwork -> [[Double]]
outputs a (ANN ne h) = scanl outputLayer a h

deltas ::  [Double]         -- Target function
        -> [[SigmoidUnit]]  -- hidden and output Units
        -> [[Double]]       -- Outputs
        -> [[Double]]       -- Deltas

deltas target _ [o]   = [zipWith (-) o target]--[ zipWith (\s y -> s * (1 - s) * (s - )) o target ]

deltas target (wh: w_hs@(w_next:_) ) (o_h : o_hs@(o_next:_) ) =
        (zipWith (*) accum o_aux) : delta_hs
    
    where delta_hs :: [[Double]]
          delta_hs@(d_next:_) = deltas target w_hs o_hs
          
          o_aux :: [Double]
          o_aux = map (\x -> x*(1-x)) o_h
          
          accum :: [Double]
          accum = foldl1 (zipWith (+)) weights_times_deltas
          
          weights_times_deltas :: [[Double]]
          weights_times_deltas = zipWith (\(SigmoidU (_:w) ) d -> map (*d) w)
                                         w_next d_next

xx = do
    nn@(ANN _ units) <- createNeuralNetwork [1,2,3,2,1]
    
    print $ deltas [0.0] units ( tail $ outputs [0.0] nn)

--updateWeights :: [[Double]] -> [[Double]] -> NeuralNetwork -> NeuralNetwork
--updateWeights deltas outputs _ = error $ (show $ length deltas) ++ " " ++ (show $ length outputs)
-- updateWeights d o (ANN i w) = ANN i (map updateW (zip d (init o)))
--     where updateW (d,x) = 
-- updateWeights [] _ (ANN i _) = ANN i []
-- updateWeights deltas outputs (ANN i x) = ANN i x
--     where 

getDiffs :: Double
         -> [Double]      -- Vector de Entrada
         -> [Double]      -- Vector de Salidas Esperadas
         -> NeuralNetwork -- Red Neural
         -> NeuralNetwork -- Red Neural de diferenciales de pesos
getDiffs alpha entry target nn = ANN input_units diff
    where (ANN input_units units) = nn
          o = entry `outputs` nn
          d = deltas target units (tail o)
          odu = zipWith (\x (y,n) -> (map (\z -> (x,z,n)) y)) (init o)
                  (map (\x -> (d,x)) units )
          entries = init o
          unitAndAlphaDelta = map (\(x,y) -> zip x (map (*alpha) y )) (zip units d)
          diff = map (\(i,u) -> map 
             (\(SigmoidU a,b) -> (SigmoidU $ map (*b) $ zipWith (*) (1:i) a)) u)
             (zip entries unitAndAlphaDelta)

addANN :: NeuralNetwork -> NeuralNetwork -> NeuralNetwork
addANN (ANN i w0) (ANN _ w1) = ANN i $ (zipWith (zipWith sumUnits) w0 w1)
    where sumUnits (SigmoidU u0) (SigmoidU u1) = SigmoidU (zipWith (-) u0 u1)

backprop :: BackPropagationExp
backprop = do
  config <- ask
  (nn,i) <- get
  let it = max_it config
  if it /= 0 && i < it 
    then do
        let training_s   = training config
        let test_s       = test config
        let new_nn = foldl' (\prev_n t -> backProp' (alpha config) prev_n t) nn training_s
        tell $ DS.singleton (0.0,0.0)
        put $ (new_nn, i + 1)
        backprop
    else
        return ()

backProp' :: Double -> NeuralNetwork -> ([Double],[Double]) -> NeuralNetwork
backProp' alpha nn@(ANN i nss) (xs,ys) = ANN i [aux (head nss) ds_hidden (1:xs),
                                                aux (nss !! 1) ds_out (1:output_hidden)]
    where output_hidden = outputLayer xs (head nss)
          output_out    = outputLayer output_hidden (nss !! 1)
          ds_out = zipWith (\s y -> s * (1-s)*(y-s)) output_out ys
          ds_hidden = zipWith (\x s -> x * (1-x) * s) output_hidden 
                          $ map (sum . zipWith (*) ds_out) . transpose $ weights !! 1
          weights = map (map (\(SigmoidU w) -> w)) nss
          
          aux :: [SigmoidUnit] -> [Double] -> [Double] -> [SigmoidUnit]
          aux l delta_l entry_l = zipWith (\(SigmoidU x) y -> SigmoidU (zipWith (+) x y))
                                        l
                                        (
                                        map (\(w,d) -> map (*(d*alpha)) w) $ 
                                            zip (replicate (length delta_l) entry_l) delta_l
                                        )
                                        --(zipWith (\x y -> alpha*x*y) delta_l out_l)
{-

backPropU alpha nss (xs, ys) = [aux (head nss) ds_hidden xs
                        ,aux (nss !! 1) ds_out output_hidden]
    where 
      ds_out = zipWithU (\s y -> s * (1 - s) * (y - s)) output_out ys
      ds_hidden = zipWithU (\x s -> x * (1-x) * s) output_hidden . toU $ map (sumU . zipWithU (*) ds_out) . map toU . transpose . map (fromU . weights) $ (nss !! 1)
      aux ns ds xs = zipWith (\n d -> n { weights = zipWithU (\w x -> w + alpha * d * x) (weights n) xs }) ns (fromU ds)

-}
backpropagation :: Double                         -- learning rate
                -> [([Double],[Double])]          -- training set
                -> [([Double],[Double])]          -- test set
                -> Int                            -- max number of iterations
                -> [Int]                          -- topology
                -> IO ()

backpropagation a tr ts i topology = do
  let se = MSupExp { training = tr, test = ts, alpha = a,
                     max_it = i}
  initial_nn <- createNeuralNetwork topology
  let (_,(s,_),w) = runRWS backprop se (initial_nn,0)
  
  print $ round $ head $ [0.0,0.0] ~~> s
  print $ round $ head $ [0.0,1.0] ~~> s
  print $ round $ head $ [1.0,0.0] ~~> s
  print $ round $ head $ [1.0,1.0] ~~> s
--   print $ [2.0,4.0] ~~> s
--   print $ [6.0,1.0] ~~> s
--   print $ [1.0,10.0] ~~> s
--   print $ [4.0,1.0] ~~> s
--   print $ [40.0,1.0] ~~> s
--   print $ [1.0,40.0] ~~> s

--andTr = [ ([i,j], [if i < j then 0.0 else 1.0]) | i <- [fromIntegral 0.. fromIntegral 10] , j <- [fromIntegral  0.. fromIntegral 10] ]
andTs = andTr
andTr = [([0.0,0.0],[1.0]),
         ([0.0,1.0],[0.0]),
         ([1.0,0.0],[0.0]),
         ([1.0,1.0],[1.0])
        ]
x = backpropagation 0.6 andTr andTs 5000 [2,10,1]

y = do
    nn <- createNeuralNetwork [1,3,1]
    print $ nn
    print $ outputs [1.0] nn