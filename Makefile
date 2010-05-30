GHC=ghc -i. --make 
all: Test TestCall

Test: *.hs Network/YAML/*.hs
	$(GHC) Test.hs

TestCall: *.hs Network/YAML/*.hs
	$(GHC) TestCall.hs

clean:
	find . -name \*.hi -delete
	find . -name \*.o -delete


