all: Test TestClient

Test: *.hs
	ghc --make Test.hs

TestClient: *.hs
	ghc --make TestClient.hs

clean:
	rm -f *.hi
	rm -f *.o


