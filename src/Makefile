all:
	ghc -package-db=.cabal-sandbox/x86_64-linux-ghc-8.0.1-packages.conf.d/ --make site.hs
	./site rebuild
	cp -R ../css/ ../img/ _site/
	./site watch
