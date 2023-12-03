all: 1

1: 1.hs
	ghc -o 1 1.hs

clean:
	rm -rf *.o *.hi 1
