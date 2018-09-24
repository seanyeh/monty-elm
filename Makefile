all: build

build: src/Monty.elm
	elm make src/Monty.elm --output www/elm.js

