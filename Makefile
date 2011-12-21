MAIN = dl

all:
	ghc -W --make -o $(MAIN) src/$(MAIN).hs -isrc

clean:
	rm src/*.hi src/*.o
