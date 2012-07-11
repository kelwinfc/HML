import System( getArgs )
import Data.List
import HML.Kmeans
import HML.PreludeHML
import qualified Data.Sequence as DS

import Control.Parallel
import Control.Parallel.Strategies

--readImage :: Int -> Int
readImage filepath = do
    s <- readFile filepath 
    let n = read s :: Int
    putStrLn $ show n
    return n

reshape :: Int -> Int -> [[Int]] -> [[[Int]]]
reshape 0    _    _ = []
reshape rows cols a = (take cols a):a'
    where a' = reshape (rows-1) cols (drop cols a)

loadData filepath = do
    s <- readFile filepath
    
    let [n,m]:content = ((map ((map read ). words) $ lines s)::[[Double]]) 
                            `using` rdeepseq
    let img = content
    return ((n,m),img)

dumpData filepath p (n,m) = do
    writeFile filepath ( show (round n) ++ " " ++ show (round m) ++ "\n"
                            ++ (concat $ intersperse "\n" $ map (concat . intersperse " " . (map (show.round))) p))

main = do
    args <- getArgs
    ((n,m),img) <- loadData (head args)
    putStrLn "Image loaded"
    let ctr = initialCentroids 3 img
    let cntr = kmeans 50 16 img
    dumpData (args !! 1) (getLeaders cntr img) (n,m)
