import HML.LogisticRegression
import Data.Packed.Vector
import qualified Data.Sequence as DS

d1 = (fromList [1,1],1)
d2 = (fromList [1,0],0)
d3 = (fromList [0,1],0)
d4 = (fromList [0,0],0)

tr = DS.fromList [d1,d2,d3,d4]

main = do
  let t = logisticRegression 0.05 0.0 tr 2 10
  print t
  print $ (fst d1)  ~> t   
  print $ (fst d2)  ~> t 
  print $ (fst d3)  ~> t   
  print $ (fst d4)  ~> t 