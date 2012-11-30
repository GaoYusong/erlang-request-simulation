
compile:
	./rebar get-deps
	./rebar compile

clean:
	@rm -fr ./deps/*
	./rebar clean

run:
	erl -pa ebin -pa deps/*/ebin -s request_simulation start

test:
	./rebar skip_deps=true eunit



