all: build

build:
	stack build

run: build
	stack exec -- run-repeat && \
	stack exec -- run-repeat-alloca && \
	stack exec -- run-repeat-loop && \
	stack exec -- run-avg && \
	stack exec -- run-avg-streaming && \
	stack exec -- run-avg-vector

build-profile:
	stack build --profile

profile: build-profile
	stack exec -- run-repeat +RTS -p -RTS; \
	stack exec -- run-repeat-alloca +RTS -p -RTS; \
	stack exec -- run-repeat-loop +RTS -p -RTS; \
	stack exec -- run-avg +RTS -p -RTS; \
	stack exec -- run-avg-streaming +RTS -p -RTS
	stack exec -- run-avg-vector +RTS -p -RTS

code:
	stack build stylish-haskell hlint intero hoogle hsc2hs && \
	zsh -c -i "code ."

core:
	stack build ghc-core-html &&  stack exec ghc-core-html -- RunRepeat.hs> RunRepeat.html

clean:
	stack clean

hoogle-server:
	stack hoogle -- server --local

.PHONY: build run build-profile profile code clean hoogle-server
