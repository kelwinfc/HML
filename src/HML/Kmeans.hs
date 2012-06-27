{-# LANGUAGE PArr , ParallelListComp #-}

import GHC.PArr
import Data.List

type Vector = [: Double :]
type Cluster = [: Vector :]

-- Calcula la distancia euclidiana entre un par de vectores.
-- Se asume que ambos vectores tienen el mismo tamaño.
dist :: Vector -> Vector -> Double
dist a b = sqrt $ sumP (zipWithP (\x y -> (x-y)^2) a b)

centroids' :: [: Cluster :] -> [: Vector :]
centroids' clusters = mapP mean clusters

mean :: Cluster -> Vector
mean a = mapP (flip (/) r) l
    where (l,r) = foldlP (\(x,n) y -> 
              (zipWithP (+) x y,n+1)) (replicateP (lengthP $ a !: 0) 0.0, 0) a
        
l = fromIntegral . lengthP

-- Calcula los centroides de la asignacion (c,p). Donde el punto p está asociado
-- al centroide c
centroids :: [: (Vector, Vector) :] -> [: Vector :]
centroids a = centroids' $ toP $ map (toP . (map snd) ) $ 
                groupBy (\x y -> fst x == fst y) (fromP a)

-- Calcula el centroide más cercano a cada punto
closestCentroids :: [: Vector :] -> [: Vector :] -> [: (Vector, Vector) :]
closestCentroids centroids = mapP (\x -> (minimumDistance x centroids, x))
    where minimumDistance p c = snd $ minimumP $ mapP (\x -> (dist p x, x)) c

{- Cada paso de kmeans debe hacer:
        - recibir los centroides ya calculados
        - calcular los closestCentroids
        - calcular los nuevos centroids
        - si convergio parar, en caso contrario recursion
-}

kmeans' :: [: Vector :] -> [: Vector :] -> [: Vector :]
kmeans' prev_centroids points = 
        if new_centroids == prev_centroids then prev_centroids
                                           else kmeans' new_centroids points
    where new_centroids = centroids $ closestCentroids prev_centroids points

part :: (Eq a) => Int -> [a] -> [[a]]
part x ys
     | zs' == [] = [zs]
     | otherwise = zs : part x zs'
    where (zs, zs') = splitAt x ys

kmeans :: Int -> [: Vector :] -> [: Vector :]
kmeans k points = kmeans' initialCentroids points
    where initialCentroids = centroids' $ toP (map toP $ part l $ fromP points)
          l = (lengthP points + k - 1) `div` k

cool :: Enum a => a -> a -> [: a :]
cool a b = [: a .. b :]

centr  = [: [: 1.0 :], [: 4.0 :] :]
points = [: [: 1.0 :], [: 2.0 :], [: 4.0 :], [: 5.0 :] :]

main = do
    putStrLn "hola"
