SOURCES := $(wildcard */*.hs) $(wildcard */*/*.hs)

.PHONY: build
build:
	stack --system-ghc --no-install-ghc build

.PHONY: build.watch
build.watch:
	ghcid --command "stack ghci --ghci-options=-fobject-code" --allow-eval --lint

.PHONY: install
install:
	stack install

.PHONY: format
format: $(SOURCES)

.PHONY: $(SOURCES)
$(SOURCES):
	hindent $@

.PHONY: test
test:
	stack test --fast

.PHONY: test.watch
test.watch:
	stack test --test --file-watch --fast

.PHONY: start
start:
	stack --system-ghc --no-install-ghc run

.PHONY: lint
lint:
	hlint .

.PHONY: fmt
fmt:
	ormolu --mode inplace $(git ls-files '*.hs')
	# this does not work

