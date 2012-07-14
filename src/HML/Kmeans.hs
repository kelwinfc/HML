module HML.Kmeans (kmeans, initialCentroids, getLeaders)
    where 

import Data.List
import qualified Data.Map as DM

import Control.Parallel
import Control.Parallel.Strategies

type Vector = [ Double ]
type Cluster = [ Vector ]

{-|
@splitInThunks@ Divide una secuencia de vectores en grupos.
-}
splitInThunks :: Int            -- ^ Tamaño de cada grupo
              -> [Vector]       -- ^ Secuencia de datos
              -> [[Vector]]     -- ^ Grupos de datos
splitInThunks n l = unfoldr go l
    where go xs = if null $ fst $ sp xs
                     then Nothing
                     else Just (sp xs)
          sp xs = splitAt n xs

{-|
@dist@ Calcula el cuadrado de la distancia euclidiana entre un par de vectores.
       Se asume que ambos vectores tienen el mismo tamaño.
-}
dist :: Vector -> Vector -> Double
dist a b = sqrt $ sum (zipWith (\x y -> (x-y)^2) a b)

{-|
@closestCentroids@ Calcula el centroide más cercano a cada punto
-}
closestCentroids :: [Vector]        -- ^ Centroides
                 -> [Vector]        -- ^ Puntos
                 -> [(Int, Vector)] -- ^ Asociacion puntos/centroide mas cercano
closestCentroids centroids = map (\x -> (nearest x (myZip centroids), x) )
    where nearest p c = fst $ snd $ minimum $ 
                          map (\(i,x) -> (dist p x, (i,x))) c
          myZip = zip [ 1 .. (length centroids) ]

{-|
@kmeans'@ Algoritmo de kmeans
-}
kmeans' :: Int              -- ^ Iteraciones restantes
        -> [ Vector ]       -- ^ Centroides previos
        -> [[Vector]]       -- ^ Grupos de datos
        -> [ Vector ]       -- ^ Centroides resultantes
kmeans' 0 ctr _ = ctr
kmeans' (n+1) prev_centroids points = 
        if new_centroids == prev_centroids then prev_centroids
                                           else kmeans' n new_centroids points
    where new_centroids = map centr ( recomputeCentroids prev_centroids points )
          
          centr :: (Vector,Int) -> Vector
          centr (c,k) = if k == 0 then c
                                  else map (\x -> x / (fromIntegral k)) c

{-|
@recomputeCentroids@ Recalcula los centrodes, moviéndolos al centro de masa de
los datos más cercanos a cada uno.
-}
recomputeCentroids :: [Vector]          -- ^ Centroides previos
                   -> [[Vector]]        -- ^ Grupos de datos
                   -> [(Vector,Int)]    -- ^ Centroides nuevos. Falta dividir
                                        -- ^ por el segundo parámetro del par
recomputeCentroids c []     = map (\x -> (replicate (length x) 0.0,0)) c
recomputeCentroids c (x:xs) = p `par` q `pseq` joinCentroids p q
    where p = recomputeCentroids c xs `using` rdeepseq
          q = newCentroids c x

newCentroids :: [Vector] -> [Vector] -> [(Vector,Int)]
newCentroids c x = zip sumc lengths
    where sumc = map (foldl' (zipWith (+))
                                   (replicate (length $ x !! 0) 0.0) )
                     clusters
          lengths = map length clusters
          clusters :: [Cluster]
          clusters = map (map snd) (fill 1 (length c) match')
          match'   = groupBy (\x y -> (fst x) == (fst y))
                             (sort $ closestCentroids c x)

          fill :: Int -> Int -> [[(Int,Vector)]] -> [[(Int,Vector)]]
          fill n maxi [] = if n == maxi then [[]]
                                        else replicate (maxi-n+1) []
          fill n maxi (first@((x,_):_):xs) = 
              if n == x then first:(fill (n+1) maxi xs)
                        else []:(fill (n+1) maxi (first:xs) )

joinCentroids :: [(Vector, Int)] -> [(Vector, Int)] -> [(Vector, Int)]
joinCentroids = zipWith (\(c0,n0) (c1,n1) -> (zipWith (+) c0 c1, n0+n1))

part :: (Eq a) => Int -> [a] -> [[a]]
part x ys
     | zs' == [] = [zs]
     | otherwise = zs : part x zs'
    where (zs, zs') = splitAt x ys

{-| @initialCentroids@ Devuelve una asignacion de centroides inicial para un
    conjunto de datos. La particion se da dividiendo uniformemente los puntos
    entre los clusters en el orden dado.
-}
initialCentroids :: Int         -- ^ Numero de centroides
                 -> [ Vector ]  -- ^ Datos a repartir
                 -> [ Vector ]  -- ^ Centroides
initialCentroids k p = centroids' (part l p)
    where l = (length p Prelude.+ k - 1) `div` k
          centroids' :: [ Cluster ] -> [ Vector ]
          centroids' clusters = map mean clusters

mean :: Cluster -> Vector
mean a = map (flip (/) r) l
    where (l,r) = foldl (\(x,n) y -> 
                  (zipWith (+) x y,n+1)) (replicate (length $ a !! 0) 0.0, 0) a

{-| Implementacion paralela del algoritmo de clustering K-means -}
kmeans :: Int           -- ^ Numero de iteraciones maximas
       -> Int           -- ^ Numero de clusters (k)
       -> [ Vector ]    -- ^ Conjunto de datos
       -> [ Vector ]    -- ^ Centroides
kmeans iterations k points = kmeans' iterations (initialCentroids k points)
                                                (splitInThunks jobSize points)
    where jobSize = 10000

getLeaders :: [Vector] -- ^ Centroides
           -> [Vector] -- ^ Puntos
           -> [Vector] -- ^ Centroides que representan cada punto
getLeaders ctr = map nearest
    where nearest p = snd $ minimum $ map (\x -> (dist p x, x)) ctr
