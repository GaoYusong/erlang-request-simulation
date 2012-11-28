
compile:
	./rebar get-deps
	./rebar compile

clean:
	@rm -fr ./deps/*
	./rebar clean

run:
	erl -pa ebin -s request_simulation start

test:
	./rebar eunit



