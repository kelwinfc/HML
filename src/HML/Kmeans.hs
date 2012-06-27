{-# LANGUAGE PArr , ParallelListComp #-}

import GHC.PArr

type Vector = [: Double :]
type Cluster = [: Vector :]

-- Calcula la distancia euclidiana entre un par de vectores.
-- Se asume que ambos vectores tienen el mismo tamaño.
dist :: Vector -> Vector -> Double
dist a b = sqrt $ sumP (zipWithP (\x y -> (x-y)^2) a b)

-- Calcula los centroides de los clusters pasados como argumento
centroids :: [: Cluster :] -> [: Vector :]
centroids clusters = mapP mean clusters
    where mean :: Cluster -> Vector
          mean x = mapP (flip (/) (l x) . sumP ) x
          l = fromIntegral . lengthP


kmeans :: Int -> [: Vector :] -> [: Cluster :]
kmeans a b = [: b :]

cool :: Enum a => a -> a -> [: a :]
cool a b = [: a .. b :]

main = do
    putStrLn "hola"
