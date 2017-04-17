.PHONY: clean
clean:
	cargo clean
	cd docs && make clean

.PHONY: build
build:
	cargo build

.PHONY: test
test:
	cargo test

.PHONY: docs
docs:
	cd docs && make html

.PHONY: docs-publish
docs-publish: docs
	surge -p docs/_build/html -d feldspar.surge.sh
