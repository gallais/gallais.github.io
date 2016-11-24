all:
	ghc -package-db=.cabal-sandbox/x86_64-linux-ghc-7.10.3-packages.conf.d/ --make site.hs
	./site rebuild
	cp -R ../css/ ../img/ _site/
	./site watch
