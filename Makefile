.PHONY: all script compile install

all: script

script: compile
	rebar3 escriptize

compile:
	rebar3 compile

install: compile
	sudo cp pkgx /usr/local/bin/pkgx