.PHONY: all shell test

all:
	erl -make

shell: all
	erl -pa ebin

test: all
	erl -pa ebin -noinput -noshell -eval "eunit:test(tests), init:stop()."
