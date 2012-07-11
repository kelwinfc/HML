-- module HML.NeuralNetworks(Neuron, LinearUnit(LinearU), StepUnit(StepU),
--                           SigmoidUnit(SigmoidU),
--                           NeuralNetwork(ANN), createNeuralNetork,
--                           (~>), (~~>))
--     where

import PreludeHML
import Data.List

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

createNeuralNetork :: [Int] -> NeuralNetwork
createNeuralNetork l@(input_size:xs) = ANN input_size a
    where a = map (\(p,c) -> replicate c (SigmoidU $ [0.0..fromIntegral p] ))
                  (zip (init l) xs)

neuralDot a b = sum $ zipWith (*) (1:a) b

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

deltas target _ [o]   = [ zipWith (\s y -> s * (1 - s) * (y - s)) o target ]

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


--updateWeights :: [[Double]] -> [[Double]] -> NeuralNetwork -> NeuralNetwork
--updateWeights deltas outputs _ = error $ (show $ length deltas) ++ " " ++ (show $ length outputs)
-- updateWeights d o (ANN i w) = ANN i (map updateW (zip d (init o)))
--     where updateW (d,x) = 
-- updateWeights [] _ (ANN i _) = ANN i []
-- updateWeights deltas outputs (ANN i x) = ANN i x
--     where 

getDiffs :: [Double]      -- Vector de Entrada
         -> [Double]      -- Vector de Salidas Esperadas
         -> NeuralNetwork -- Red Neural
         -> NeuralNetwork -- Red Neural de diferenciales de pesos
getDiffs entry target nn = ANN input_units diff
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

alpha = 0.1





--x = updateWeights d outs nn
    
          
          
          
{-
add_delta :: SigmoidUnit -> Double -> [Double] -> SigmoidUnit
add_delta (SigmoidU weights) alpha delta = 
    SigmoidU (zipWith (\w d -> w + alpha*d) weights delta)

new_weights :: NeuralNetwork -> Double -> [[[Double]]] -> NeuralNetwork
new_weights (ANN l []) alpha [] = ANN l []
new_weights (ANN l (h:hs)) alpha (dl:dls) = ANN l (new_layer:aux)
    where new_layer = zipWith go h dl
          go n d = add_delta n alpha d
          ANN _ aux = new_weights (ANN l hs) alpha dls
-}

{-
backProp :: Double -> NeuralNetwork -> [([Double],Double)] -> NeuralNetwork
backProp alpha nn@(ANN _ w) tr = nn--new_weights nn alpha d
    where o = map (\(x,y) -> new_weights nn alpha (deltas y w (outputs x nn))) tr
          --d = map (\y -> deltas y w o) ys
          -}
{- procesar la capa --> llamar a la siguiente
                    <-- calcular el delta
                    --- calcular los nuevos pesos
-}
{-
    -- | Train the given neural network using the backpropagation algorithm on the given sample with the given learning ratio (alpha)
    backPropU :: Double -> [[Neuron]] -> (UArr Double, UArr Double) -> [[Neuron]]
    backPropU alpha nss (xs, ys) = [aux (head nss) ds_hidden xs
                        ,aux (nss !! 1) ds_out output_hidden]
    
    where 
      output_hidden = computeLayerU (head nss) xs
      output_out = computeLayerU (nss !! 1) output_hidden
      
      ds_out = zipWithU (\s y -> s * (1 - s) * (y - s)) output_out ys
      ds_hidden = zipWithU (\x s -> x * (1-x) * s) output_hidden . toU $ map (sumU . zipWithU (*) ds_out) . map toU . transpose . map (fromU . weights) $ (nss !! 1)
      
      aux ns ds xs = zipWith (\n d -> n { weights = zipWithU (\w x -> w + alpha * d * x) (weights n) xs }) ns (fromU ds)

-}