check: test elvis types

test: eunit

eunit:
	rebar3 eunit

elvis:
	elvis rock

types:
	rebar3 as dialyzer dialyzer