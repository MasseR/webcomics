.PHONY: setup build test

all: setup build test

setup:
	stack setup

build: setup
	stack build

test: setup
	stack test
