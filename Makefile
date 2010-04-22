all: Test TestCall

Test: *.hs Network/YAML/*.hs
	ghc -i. --make Test.hs

TestCall: *.hs Network/YAML/*.hs
	ghc -i. --make TestCall.hs

clean:
	find . -name *.hi -delete
	find . -name *.o -delete


