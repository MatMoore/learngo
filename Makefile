.PHONY=clean

docs/index.html: clean
	mkdir -p docs
	elm make main.elm --output docs/index.html

clean:
	rm -f docs/*
