import HML.Prelude
import HML.NeuralNetworks
import Data.List

main = do
    --putStrLn $ show $ [1,2,3] ~> LinearU [1,2,3,4]
    let n = 5*10**7
    putStrLn $ show $ mse [1..n] [5..n+5]
    --putStrLn $ show $ 0.5 * (foldl' (\a (x,y) -> a + (x-y)*(x-y)) 0.0 (zip [1..n] [5..n+5]))
