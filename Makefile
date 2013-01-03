SOURCES = src/*

all: latc

latc: $(SOURCES)
	mkdir -p build
	ghc -cpp -DRUNTIME_PATH=\"$(CURDIR)/lib/runtime.o\" --make src/Latter.hs -isrc/ -ilib/ -ilib/temporary/ -outputdir build -tmpdir build -o latc

clean:
	rm -rf build/*
	rm -f latc
