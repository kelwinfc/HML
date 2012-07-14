---------------- EXAMPLE 

f x = (fromList [x],-2+3*x)
g x = (fromList [x],3+2*x)
k x y = (fromList [x,y],3+2*x+y)
(<<) x y = if x < y then (fromList[x,y],1.0) else (fromList[x,y],0.0)

menorTR :: Seq (Vector Double, Double)
menorTR = DS.fromList [ i << j | i <- [fromIntegral 0.. fromIntegral 10] , j <- [fromIntegral  0.. fromIntegral 10] ]

menorTS :: Seq (Vector Double, Double)
menorTS = DS.fromList [ i << j | i <- [fromIntegral 11.. fromIntegral 30] , j <- [fromIntegral  11.. fromIntegral 20] ]

prueba_tr :: Seq (Vector Double, Double)
prueba_tr = DR.fmap g $ DS.fromList [0.0,2.0,3.0,5.0]

prueba_ts :: Seq (Vector Double, Double)
prueba_ts = DR.fmap g $ DS.fromList [10.0,12.0,13.0,15.0]

training__set :: Seq (Vector Double, Double)
training__set = DR.fmap f $ DS.fromList [0.0..100.0]

test__set :: Seq (Vector Double, Double)
test__set = DR.fmap f $ DS.fromList [200.0..250.0]