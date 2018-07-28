all:
	stack build
clean:
	stack clean
start:
	stack exec shinjirecs-api-exe
migrate:
	stack exec shinjirecs-api-migrate
tests:
	stack exec shinjirecs-api-migrate test && stack test
docs:
	stack haddock -o ./docs
#	echo $(abspath ./doc)
