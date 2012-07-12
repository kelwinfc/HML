import Data.Packed.Vector
import qualified Data.Sequence as DS
import qualified Data.Functor as DR
import HML.LinearRegression
import HML.Regression

datos1 :: Vector Double
datos1 = fromList [158,39,36,68,43,55]
datos2 :: Vector Double
datos2 = fromList [152,38,34,66,40,55]
datos3 :: Vector Double
datos3 = fromList [168,43,39,72.5,41,54.5]
datos4 :: Vector Double
datos4 = fromList [159,40,36,68.5,42,57]

datos5 :: Vector Double
datos5 = fromList [158,41,36,68.5,44,57]
datos6 :: Vector Double
datos6 = fromList [164,40,36,71,44.5,54]
datos7 :: Vector Double
datos7 = fromList [156,41,36,67,36,56]
datos8 :: Vector Double
datos8 = fromList [167,44,37,73,41.5,58]

datos_tr = DS.fromList [(datos1,43),(datos2,45),(datos3,48),(datos4,49)]
datos_ts = DS.fromList [(datos5,50),(datos6,51),(datos7,52),(datos8,52)]


f x = (fromList[x,x*x],4*x*x + 3*x + 2)

tr :: DS.Seq (Vector Double,Double)
tr = fmap f $ DS.fromList [fromIntegral 0 .. fromIntegral 10] 

ts :: DS.Seq (Vector Double,Double)
ts = fmap f $ DS.fromList [fromIntegral 11 .. fromIntegral 20] 

--one (x,y) = (join [fromList [1],x],y)

main = linearRegressionGDWithStats 0.9 0.00 tr ts 2 20
