.PHONY: all build deploy rebuild

all: build

build:
	cabal run -- build

rebuild:
	cabal run -- rebuild

deploy:
	./deploy.sh
