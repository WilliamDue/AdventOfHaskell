GHC=ghc

.PHONY: run clean

part%:
	@$(GHC) $@.hs -o $@.run
	@rm *.o *.hi

clean:
	@rm *.run
