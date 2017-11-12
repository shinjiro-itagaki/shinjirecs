JS=index.js

all: compile-css compile
start:
compile:
	elm-make src/Main.elm --output=public/$(JS)
compile-css:
	elm-css src/MainCss.elm --output=public --module=MainCss
clean:
	rm public/*.js public/*.css
