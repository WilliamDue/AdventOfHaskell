GHC=ghc
ARGS = -O2 -threaded

.PHONY: run clean

part%:
	@$(GHC) $(ARGS) $@.hs -o $@.run
	@rm *.o *.hi

clean:
	@rm *.run
