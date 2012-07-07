module HML.Prelude where

data SupervisedExperiment = SupExp { training_set  :: [(Double,Double)],
                                     test_set      :: [(Double,Double)],
                                     learning_rate :: Double,
                                     iterations    :: Int
                                   }
