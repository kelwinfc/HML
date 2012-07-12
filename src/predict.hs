import System( getArgs )
import Data.List
import HML.NeuralNetworks
import HML.PreludeHML
import qualified Data.Sequence as DS

import Control.Parallel
import Control.Parallel.Strategies

readImage filepath = do
    s <- readFile filepath 
    let n = read s :: Int
    putStrLn $ show n
    return n

loadData filepath = do
    s <- readFile filepath
    
    let content = ((map ((map read ). words) $ lines s)::[[Double]]) 
                    `using` rdeepseq
    let aux = map (\x -> splitAt (length x - 1) x) content
    
    return aux

dumpData filepath p (n,m) = do
    writeFile filepath ( show (round n) ++ " " ++ show (round m) ++ "\n"
                            ++ (concat $ intersperse "\n" $
                        map (concat . intersperse " " . (map (show.round))) p))

main = do
    args <- getArgs
    imgs <- loadData (head args)
    
    -- Cargar datos
    let (training_set, test_set) = splitAt (length imgs `div` 2) imgs
    
    -- Entrenar
    neural_network <- backpropagation "experimento.png"
                                     0.05 training_set test_set 200
                                     [length $ fst $ training_set !! 0,
                                      10,
                                      length $ snd $ training_set !! 0]
    
    -- Reportar porcentaje de clasificacion
    let classif  = map (\(x,y) -> (x ~~> neural_network, y)) training_set
    let _error   = map (\(x,y) -> (map round x , map round y)) classif
    let rate     = filter (\(x,y) -> x == y) _error
    
    putStr "Rate: "
    putStr $ show $ (fromIntegral (length rate))*100.0 / 
                     (fromIntegral (length classif)
                    )
    putStrLn "%"
    