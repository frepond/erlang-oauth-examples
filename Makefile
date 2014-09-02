DEPS_PLT=$(CURDIR)/.deps_plt
DEPS=erts kernel stdlib xmerl inets ssl crypto public_key 

deps:
	@rebar get-deps

update-deps:
	@rebar update-deps

all: compile

compile:
	@test -d ebin || mkdir ebin
	@erl -make

clean:
	@rm -rf ebin/*.beam erl_crash.dump

$(DEPS_PLT):
	@echo Building local plt at $(DEPS_PLT)
	@echo
	@dialyzer --output_plt $(DEPS_PLT) --build_plt --apps $(DEPS) -r deps

dialyzer: compile $(DEPS_PLT)
	@dialyzer --fullpath --plt $(DEPS_PLT) -Wrace_conditions -Wunmatched_returns -r ./ebin deps/*/ebin

typer:
	@typer -I deps -pa deps/*/ebin -pa ebin --plt $(DEPS_PLT) --annotate -r ./src
