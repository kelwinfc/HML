doc: src/HML/Kmeans.hs src/HML/NeuralNetworks.hs src/HML/PreludeHML.hs src/HML/Regression.hs src/HML/SupportVectorMachines.hs src/HML/LinearRegression.hs src/HML/LogisticRegression.hs src/HML/Regression.hs
	haddock --use-unicode -h -o doc src/HML/NeuralNetworks.hs src/HML/PreludeHML.hs src/HML/SupportVectorMachines.hs src/HML/Kmeans.hs src/HML/LinearRegression.hs src/HML/LogisticRegression.hs src/HML/Regression.hs

compress:
	ghc --make -o ../bin/compress -threaded compress.hs

pruebaANN:
	ghc --make -o ../bin/pruebaANN -threaded pruebaANN.hs

pruebaSVM:
	ghc --make -o ../bin/pruebaSVM -threaded pruebaSVM.hs

operadores:
	ghc --make -o ../bin/operadores operadores.hs

peso:
	ghc --make -o ../bin/peso peso.hs

clean:
	rm -rf bin/* *~ src/*~

cleandoc:
	rm doc/*.gif doc/*.css doc/*.html doc/*.js

clean:
	 rm -Rf *~ src/*~ test/*~ other/show other/convert
