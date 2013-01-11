SOURCES = src/*

all: latc_x86

latc_x86: $(SOURCES)
	mkdir -p build
	ghc -cpp -DRUNTIME_PATH=\"$(CURDIR)/lib/runtime.o\" --make src/Latter.hs -isrc/ -ilib/ -ilib/temporary/ -outputdir build -tmpdir build -o latc_x86

clean:
	rm -rf build/*
	rm -f latc_x86
