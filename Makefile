all: 1 2 3

1: 1.hs
	ghc -o 1 1.hs

2: 2.hs
	ghc -o 2 2.hs

3: 3.hs
	ghc -o 3 3.hs

clean:
	rm -rf *.o *.hi 1 2 3
