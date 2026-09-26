# steer: build, test, package.   make | make test | make dist | make install PREFIX=~/.local
RACKET ?= racket
RACO   ?= raco
PREFIX ?= $(HOME)/.local
SRC    := $(wildcard steer/*.rkt)
SKILLS := $(shell find skills -type f -not -name '.*')

.PHONY: all build test test-bin dist install clean

all: build

build: build/steer

# skills.rkt embeds skills/ at compile time. raco make decides by content hash, so a touch is not
# enough to notice added or removed skill files: drop its compiled form instead.
build/steer: $(SRC) $(SKILLS)
	@mkdir -p build
	rm -f steer/compiled/skills_rkt.zo steer/compiled/skills_rkt.dep
	$(RACO) make -v steer/main.rkt
	$(RACO) exe -o build/steer steer/main.rkt

test:
	$(RACO) make steer/main.rkt
	$(RACO) test tests

# the same end-to-end suite against the compiled executable
test-bin: build/steer
	STEER_BIN=$(CURDIR)/build/steer $(RACO) test tests/cli-test.rkt

# self-contained directory (binary + runtime libs) that works without this checkout
dist: build/steer
	rm -rf dist
	$(RACO) distribute dist build/steer

install: dist
	mkdir -p $(PREFIX)/bin
	ln -sf $(CURDIR)/dist/bin/steer $(PREFIX)/bin/steer
	@echo "installed $(PREFIX)/bin/steer -> $(CURDIR)/dist/bin/steer"

clean:
	rm -rf build dist steer/compiled tests/compiled
