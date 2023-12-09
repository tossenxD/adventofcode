all: 1 2 3 4 5 6

1: 1.hs
	ghc -o 1 1.hs

2: 2.hs
	ghc -o 2 2.hs

3: 3.hs
	ghc -o 3 3.hs

4: 4.hs
	ghc -o 4 4.hs

5: 5.hs
	ghc -o 5 5.hs

6: 6.hs
	ghc -o 6 6.hs

clean:
	rm -rf *.o *.hi 1 2 3 4 5 6
