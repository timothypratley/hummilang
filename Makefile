.PHONY: all setup update clean dev serve open test

ifeq ($(shell uname -s),Linux)
  OPEN := xdg-open
else
  OPEN := open
endif

all: setup test min

setup:
	clojure -P

update:
	clojure -M:outdated --every --write

clean:
	rm -rf target/public

test:
	clojure -M:test-clj
	clojure -M:serve:test-cljs

min: clean
	clojure -M:build

dev:
	clojure -M:serve:dev

cards:
	clojure -M:serve:dev:cards

serve: clean
	clojure -M:serve
