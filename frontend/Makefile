SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c

ps-sources := $(shell fd -epurs)
bundle = spago bundle-module -m $(1) --to output.js
ps-bundle = $(call bundle,Main)
js-bundle = $(call bundle,SdkApi)
build = $(1) && BROWSER_RUNTIME=1 webpack --mode=production --entry $(2)

ps-dev:
	@${ps-bundle} && BROWSER_RUNTIME=1 webpack-dev-server --progress --entry ./main.js

ps-build:
	@$(call build,${ps-bundle},./main.js)

js-build:
	@$(call build,${js-bundle},./index.js)

check:
	@purs-tidy check ${ps-sources}
	spago build --purs-args "--stash --censor-lib --censor-codes=UserDefinedWarning --strict"

format:
	@purs-tidy format-in-place ${ps-sources}
