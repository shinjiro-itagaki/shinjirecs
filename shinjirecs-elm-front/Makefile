JS=index.js
SERVER=../shinjirecs-rails-server/public/
all: compile-css compile deploy
start:
compile:
	elm-make src/Main.elm --output=public/$(JS)
compile-css:
	elm-css src/MainCss.elm --output=public --module=MainCss
clean:
	rm public/*.js public/*.css
deploy:
	cp public/index.html $(SERVER)
	cp public/*.js       $(SERVER)
	cp public/*.css      $(SERVER)
