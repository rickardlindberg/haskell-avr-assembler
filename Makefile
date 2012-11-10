all: test

Instructions.hs: Instructions.spec
	runhaskell Parser.hs < Instructions.spec > Instructions.hs

test: Instructions.hs
	runhaskell Properties.hs

.PHONY: test
