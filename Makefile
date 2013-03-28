all: clean
	rebar --verbose compile

quick:
	rebar --verbose compile

clean:
	rebar clean

doc: quick
	rebar skip_deps=true doc
	javadoc -overview doc/overview-summary.html \
			-classpath ./ebin:/usr/local/lib/erlang/lib/jinterface-1.5.8/priv/OtpErlang.jar \
			-verbose -d doc/java -use -version -author `find java_src -name *.java`