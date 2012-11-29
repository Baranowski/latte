SOURCES = src/*

all: latc

latc: $(SOURCES)
	mkdir -p build
	ghc -cpp -DJASMIN_DIR=\"$(CURDIR)/lib\" --make src/Latter.hs -isrc/ -ilib/ -ilib/temporary/ -outputdir build -tmpdir build -o latc

clean:
	rm -rf build/*
	rm -f latc
