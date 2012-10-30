SOURCES = src/*

all: latc

latc: $(SOURCES)
	mkdir -p build
	ghc --make src/Latter.hs -isrc/ -outputdir build -tmpdir build -o latc

clean:
	rm -f build/*
	rm -f latc
