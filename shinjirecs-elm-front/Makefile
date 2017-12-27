JS=index.js
SERVER=../shinjirecs-rails-server/public/
all: compile deploy
start:
compile:
	elm-make src/Main.elm --output=public/$(JS)
clean:
	rm public/*.js
deploy:
	cp public/index.html $(SERVER)
	cp public/*.js       $(SERVER)
