import System( getArgs )
import Data.List
import HML.SupportVectorMachines
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

do_experiment kernel file_data rate iterations = do
    dataset <- loadData file_data
    
    -- Cargar datos
    let (training_set, test_set) = getTrainingAndTestSets dataset rate
    
    -- Entrenar
    let svm = train kernel iterations 0.0 (DS.fromList training_set)
    
    -- Reportar porcentaje de clasificacion
    let ts_classif  = map (\(x,y) -> (x ~> svm, y)) test_set
    let ts_error   = map (\(x,y) -> (map round x , map round y)) ts_classif
    let ts_rate     = filter (\(x,y) -> x == y) ts_error
    
    let tr_classif  = map (\(x,y) -> (x ~> svm, y)) training_set
    let tr_error   = map (\(x,y) -> (map round x , map round y)) tr_classif
    let tr_rate     = filter (\(x,y) -> x == y) tr_error
    
    putStr "    Training Rate: "
    putStr $ show $ (fromIntegral (length tr_rate))*100.0 / 
                     (fromIntegral (length tr_classif))
    putStrLn "%"
    
    putStr "    Test Rate: "
    putStr $ show $ (fromIntegral (length ts_rate))*100.0 / 
                    (fromIntegral (length ts_classif))
    putStrLn "%"

main = do
    putStrLn "Experimento de la función <"
    do_experiment id "../test/minmax.data" 75 5
    putStrLn ""
    
    putStrLn "Experimento de la función circulo"
    do_experiment id "../test/circle.data" 10 100
    putStrLn ""
    
    putStrLn "Experimento de la función par"
    do_experiment id "../test/par.data" 100 50
    putStrLn ""

    putStrLn "Experimento de la función par"
    do_experiment id "../test/par.data" 100 50
    putStrLn ""
    
    putStrLn "Experimento del dataset \"Iris\""
    putStrLn "http://archive.ics.uci.edu/ml/datasets/Iris"
    do_experiment (rbfKernel 1.0 . polyKernel 10 ) "../test/iris.data" 100 50
    putStrLn ""

