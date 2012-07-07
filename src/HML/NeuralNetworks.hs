module HML.NeuralNetworks(Neuron, LinearUnit(LinearU), StepUnit(StepU),
                          SigmoidUnit(SigmoidU),
                          MultiLayerPerceptron, createNeuralNetork,
                          (~>))
    where

import HML.Prelude

class Neuron a where
    
    (~>) :: [Double] -> a -> Double
    (~>) entries neuron = neuron <~ entries
    
    (<~) :: a -> [Double] -> Double
    (<~) neuron entries = entries ~> neuron

data LinearUnit = LinearU [Double]

instance Neuron LinearUnit where
    (~>) entries (LinearU weights) = neuralDot entries weights

data StepUnit = StepU Double [Double]

instance Neuron StepUnit where
    (~>) entries (StepU theta weights) = 
            if (neuralDot entries weights) >= theta then 1.0
                                                    else 0.0

data SigmoidUnit = SigmoidU [Double]

instance Neuron SigmoidUnit where
    (~>) entries (SigmoidU weights) =
        1.0 / ( 1.0 + (exp (- (neuralDot entries weights) )))

data MultiLayerPerceptron = ANN [LinearUnit] [[SigmoidUnit]]

createNeuralNetork :: [Int] -> MultiLayerPerceptron
createNeuralNetork (x:xs) = ANN [] [[]]

neuralDot a b = sum $ zipWith (*) (1:a) b
