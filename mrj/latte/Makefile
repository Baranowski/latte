all: latc

latc:
	mkdir -p build
	ghc --make src/Latter.hs -outputdir build -tmpdir build -o latc

clean:
	rm -f build/*
	rm -f latc
