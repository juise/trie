.PHONY: all compile run test clean
.PHONY: dialyzer

REBAR=./rebar3

all: $(REBAR) compile

compile:
		$(REBAR) compile

run:
		erl -pa _build/default/lib/*/ebin -boot start_sasl

test:
		$(REBAR) eunit skip_deps=true verbose=3
		$(REBAR) ct skip_deps=true verbose=3

clean:
		$(REBAR) clean
		rm -rf ./log
		rm -rf ./erl_crash.dump

dialyzer:
		$(REBAR) dialyzer

