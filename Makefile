.PHONY=elm live

elm:
	elm make main.elm --output docs/elm.js

live:
	elm-live --open --dir=docs main.elm -- --output docs/elm.js
