.PHONY: all build deploy

all: build

build:
	cabal run -- build

deploy:
	./deploy.sh
