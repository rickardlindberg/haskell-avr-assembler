all: test

Instructions.hs: Instructions.spec Parser.hs Parser/*.hs
	runhaskell Parser.hs < Instructions.spec > Instructions.hs

test: Instructions.hs
	runhaskell Properties.hs

.PHONY: test
