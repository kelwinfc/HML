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

loadData = do
    --args <- getArgs
    let args = ["../test/rott.img"]
    s <- readFile $ args !! 0
    
    let [n,m]:content = ((map ((map read ). words) $ lines s)::[[Double]]) 
                            `using` rdeepseq
    let img = content
    return img

main = do
    img <- loadData
    
    putStrLn "Image loaded"
    let ctr = initialCentroids 3 img
    let cntr = kmeans 100 16 img
    putStrLn $ show cntr
