REBAR?=./rebar
all: clean
	${REBAR} --verbose compile

quick:
	${REBAR} --verbose compile

clean:
	${REBAR} clean

doc: quick
	${REBAR} skip_deps=true doc
	javadoc -overview doc/overview-summary.html \
			-classpath ./priv/OtpErlang.jar \
			-verbose -d doc/java -use -version -author `find java_src -name *.java`
