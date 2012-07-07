
module HML.Kmeans where

import Data.List
import qualified Data.Map as DM

type Vector = [ Double ]
type Cluster = [ Vector ]

-- Calcula la distancia euclidiana entre un par de vectores.
-- Se asume que ambos vectores tienen el mismo tamaño.
dist :: Vector -> Vector -> Double
dist a b = sqrt $ sum (zipWith (\x y -> (x-y)^2) a b)

centroids' :: [ Cluster ] -> [ Vector ]
centroids' clusters = map mean clusters

mean :: Cluster -> Vector
mean a = map (flip (/) r) l
    where (l,r) = foldl (\(x,n) y -> 
              (zipWith (+) x y,n+1)) (replicate (length $ a !! 0) 0.0, 0) a

l = fromIntegral . length

-- Calcula los centroides de la asignacion (c,p). Donde el punto p está asociado
-- al centroide c
--centroids :: [: (Vector, Vector) :] -> [: Vector :]
centroids l = map ((\(x,y)-> map (flip (/) (fromIntegral x)) y) . snd) assoc
    where assoc = DM.toList $ getAcum l
          getAcum :: Ord k => Num a => [ (k,[a]) ] -> DM.Map k (Int,[a])
          getAcum = DM.fromListWith (\ (n1,v1) (n2,v2) -> 
                                              (n1+n2,zipWith (+) v1 v2)) 
                                         . map (\(x,y) -> (x,(1,y)))

-- Calcula el centroide más cercano a cada punto
--closestCentroids :: [: Vector :] -> [: Vector :] -> [: (Vector, Vector) :]
closestCentroids centroids = map (\x -> (nearest x (myZip centroids), x) )
    where nearest p c = fst $ snd $ minimum $ map (\(i,x) -> (dist p x, (i,x))) c
          myZip centroids = zip [ 1 .. (length centroids) ] centroids

{- Cada paso de kmeans debe hacer:
        - recibir los centroides ya calculados
        - calcular los closestCentroids
        - calcular los nuevos centroids
        - si convergio parar, en caso contrario recursion
-}

kmeans' :: Int -> [ Vector ] -> [ Vector ] -> [ Vector ]
{-
kmeans' 0 ctr _ = ctr
kmeans' (n+1) prev_centroids points = 
        if new_centroids == prev_centroids then prev_centroids
                                           else kmeans' n new_centroids points
    where new_centroids = centroids $ closestCentroids prev_centroids points
-}
kmeans' 0 ctr _ = ctr
kmeans' (n+1) prev_centroids points = kmeans' n new_centroids points
    where new_centroids = centroids $ closestCentroids prev_centroids points

part :: (Eq a) => Int -> [a] -> [[a]]
part x ys
     | zs' == [] = [zs]
     | otherwise = zs : part x zs'
    where (zs, zs') = splitAt x ys

initialCentroids k p = centroids' (part l p)
    where l = (length p Prelude.+ k - 1) `div` k

kmeans :: Int -> Int -> [ Vector ] -> [ Vector ]
kmeans iterations k points = kmeans' iterations (initialCentroids k points) points

cool :: Enum a => a -> a -> [ a ]
cool a b = [ a .. b ]

centr  = [ [ 1.0, 1.0 ], [ 4.0, 4.0 ], [10.0, 10.0] ]
points = [ [ 1.0, 0.0 ], [ 2.0, 1.0 ], [ 4.0, 5.0 ], [ 5.0, 6.0 ] ]
