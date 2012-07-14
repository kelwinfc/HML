import HML.LogisticRegression
import HML.PreludeHML
import HML.Regression
import Data.Packed.Vector
import qualified Data.Sequence as DS

or_tr = DS.fromList [(fromList [1,1],1),(fromList [1,0],1),(fromList [0,1],1),(fromList [0,0],0)]
and_tr = DS.fromList [(fromList [1,1],1),(fromList [1,0],0),(fromList [0,1],0),(fromList [0,0],0)]

main = do
  let a = LogisticR $ logisticRegression 0.01 0.0 and_tr 2 10
  let o = LogisticR $ logisticRegression 0.01 0.0 or_tr 2 10
  
  -- (1 and 0) or 1 = 1
  let exp1 = [head $ [1,0] ~> a,1] ~> o
  print exp1
  
  let exp2 = [head $ [1,1] ~> o,head $ [0,1] ~> o] ~> a
  print exp2