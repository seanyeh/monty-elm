all: build

build: src/Monty.elm
	elm make src/Monty.elm --output docs/elm.js

