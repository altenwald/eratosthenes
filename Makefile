all: compile

clean-devel: clean
	-rm -rf _build

clean:
	./rebar3 clean

compile:
	./rebar3 compile

test:
	./rebar3 eunit
	./rebar3 cover
	./covertool \
		-cover _build/test/cover/eunit.coverdata \
		-appname ephp \
		-output cobertura.xml

shell:
	./rebar3 shell

.PHONY: test compile all shell
