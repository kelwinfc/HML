module HML.Kmeans where 

import Data.List
import qualified Data.Map as DM

import Control.Parallel
import Control.Parallel.Strategies

type Vector = [ Double ]
type Cluster = [ Vector ]


splitInThunks :: Int -> [Vector] -> [[Vector]]
splitInThunks n l = unfoldr go l
    where go xs = if null $ fst $ sp xs
                     then Nothing
                     else Just (sp xs)
          sp xs = splitAt n xs

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
closestCentroids :: [Vector] -> [Vector] -> [(Int, Vector)]
closestCentroids centroids = map (\x -> (nearest x (myZip centroids), x) )
    where nearest p c = fst $ snd $ minimum $ map (\(i,x) -> (dist p x, (i,x))) c
          myZip centroids = zip [ 1 .. (length centroids) ] centroids

kmeans' :: Int              -- Iteraciones restantes
        -> [ Vector ]       -- Centroides
        -> [[Vector]]       -- Thunk de datos
        -> [ Vector ]       -- Centroides resultantes
kmeans' 0 ctr _ = ctr
kmeans' (n+1) prev_centroids points = 
        if new_centroids == prev_centroids then prev_centroids
                                           else kmeans' n new_centroids points
    where new_centroids = map centr centrAndLength
          centrAndLength = recomputeCentroids prev_centroids points
          centr :: (Vector,Int) -> Vector
          centr (c,k) = if k == 0 then c
                                  else map (\x -> x / (fromIntegral k)) c


recomputeCentroids :: [Vector] -> [[Vector]] -> [(Vector,Int)]
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


{-
kmeans' 0 ctr _ = ctr
kmeans' (n+1) prev_centroids points = kmeans' n new_centroids points
    where new_centroids = centroids $ closestCentroids prev_centroids points
    -}

part :: (Eq a) => Int -> [a] -> [[a]]
part x ys
     | zs' == [] = [zs]
     | otherwise = zs : part x zs'
    where (zs, zs') = splitAt x ys

initialCentroids k p = centroids' (part l p)
    where l = (length p Prelude.+ k - 1) `div` k

jobSize = 10000
kmeans :: Int -> Int -> [ Vector ] -> [ Vector ]
kmeans iterations k points = kmeans' iterations (initialCentroids k points)
                                                (splitInThunks jobSize points)

centr  = [ [ 1.0, 1.0 ], [ 4.0, 4.0 ], [10.0, 10.0] ]
points = [ [ 1.0, 0.0 ], [ 2.0, 1.0 ], [ 4.0, 5.0 ], [ 5.0, 6.0 ] ]
