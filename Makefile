SOURCE:=$(wildcard *.hs)

all: $(SOURCE)
	ghc --make Main
