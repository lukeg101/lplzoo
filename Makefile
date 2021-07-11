
LANGS = ULC SKI STLC SystemT PCF SystemF SOL Cata Ana Sub Omega FOmega LF C

BINS = cabal ghc hlint

# Check necessary binaries exist when running makefile
.ONESHELL:
.PHONY: check-bins-exists
.SHELLFLAGS = -ec
check-bins-exists:
	@$(foreach bin, $(BINS), command -v $(bin) > /dev/null || { echo '[ERROR]: $(bin) not in scope of Makefile! is it on your PATH?'; exit 1; };)
	@echo '[INFO]: Bins exist!'

# Check linting, for one language
.ONESHELL:
.PHONY: quality-check-one
.SHELLFLAGS = -ec
quality-check-one:
	@echo '[INFO]: linting $(LANGUAGE)'; hlint $(LANGUAGE); echo '[INFO]: Linting Succeeded!'

# Check linting, docs all good
.ONESHELL:
.PHONY: quality-check
.SHELLFLAGS = -ec
quality-check:
	@$(foreach lang, $(LANGS), echo '[INFO]: linting $(lang)'; hlint $(lang); )
	@echo '[INFO]: Linted all!'

build: check-bins-exists
	cabal build $(LANGUAGE)
	cabal test test-$(LANGUAGE)
	make quality-check-one $(LANGUAGE)
	make docs LANGS=$(LANGUAGE)
	@echo '[INFO]: Building $(LANGUAGE) Succeeded!'

build-all: check-bins-exists
	cabal build
	make test
	make quality-check
	make docs

run: check-bins-exists
	cabal run $(LANGUAGE)

test: check-bins-exists
	cabal test

check-deps: check-bins-exists
	cabal update

docs: check-bins-exists
	@$(foreach lang, $(LANGS), echo '[INFO]: Documenting $(lang)'; cabal haddock $(lang); )

clean:
	cabal clean
