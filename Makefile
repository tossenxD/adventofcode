all: 1 2

1: 1.hs
	ghc -o 1 1.hs

2: 2.hs
	ghc -o 2 2.hs

clean:
	rm -rf *.o *.hi 1 2
