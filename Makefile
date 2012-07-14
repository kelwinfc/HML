doc: src/HML/Kmeans.hs src/HML/NeuralNetworks.hs src/HML/PreludeHML.hs src/HML/Regression.hs src/HML/SupportVectorMachines.hs
	haddock --use-unicode -h -o doc src/HML/NeuralNetworks.hs src/HML/PreludeHML.hs src/HML/SupportVectorMachines.hs src/HML/Kmeans.hs

clean:
	rm -rf bin/* *~ src/*~

cleandoc:
	rm doc/*.gif doc/*.css doc/*.html doc/*.js

clean:
	 rm -Rf *~ src/*~ test/*~ other/show other/convert
