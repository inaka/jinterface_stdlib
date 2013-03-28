all: clean
	rebar --verbose compile

quick:
	rebar --verbose compile

clean:
	rebar clean

doc: quick
	rebar skip_deps=true doc