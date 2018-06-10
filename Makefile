all: build

build:
	stack build

run: build
	stack exec -- run-repeat && \
	stack exec -- run-avg && \
	stack exec -- run-avg-streaming

build-profile:
	stack build --profile

profile: build-profile
	stack exec -- run-repeat +RTS -p -RTS; \
	stack exec -- run-avg +RTS -p -RTS; \
	stack exec -- run-avg-streaming +RTS -p -RTS

code:
	stack build stylish-haskell hlint intero hoogle hsc2hs && \
	zsh -c -i "code ."

.PHONY: build run build-profile profile code 
