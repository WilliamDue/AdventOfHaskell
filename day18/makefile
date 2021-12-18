GHC=ghc
ARGS = -O3

.PHONY: run clean

part1and2.run: part1and2.hs
	@$(GHC) $(ARGS) $< -o $@
	@rm *.o *.hi

run: part1and2.run
	./$<

clean:
	@rm *.run
