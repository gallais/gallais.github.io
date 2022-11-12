all:
	ghc --make site.hs
	./site rebuild
	cp -R ../css/ ../img/ _site/
	./site watch
