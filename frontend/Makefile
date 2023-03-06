SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c

ps-sources := $(shell fd -epurs)
bundle = spago bundle-module -m $(1) --to output.js
js-bundle = $(call bundle,SdkApi)
build = $(1) && BROWSER_RUNTIME=$(2) webpack --mode=production --entry $(3)

bundle:
	@$(call bundle,SdkApi)

check:
	@purs-tidy check ${ps-sources}
	spago build --purs-args "--stash --censor-lib --censor-codes=UserDefinedWarning --strict"

format:
	@purs-tidy format-in-place ${ps-sources}
