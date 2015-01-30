REBAR = $(shell pwd)/rebar
VSN = $(shell cat src/*.app.src | grep vsn | sed 's/^[^"]*"//' | sed 's/".*$//')

.PHONY: deps all test

all: cp-hooks deps compile

cp-hooks:
	cp hooks/* .git/hooks

compile:
	$(REBAR) compile

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean

long-test:
	-rm -r .eunit
	$(REBAR) skip_deps=true -DEQC_LONG_TESTS eunit -v -r

test: 
	$(REBAR) compile
	-rm -r .eunit
	$(REBAR) eunit skip_deps=true -r -v

quick-test:
	-rm -r .eunit
	$(REBAR) -DEQC_SHORT_TEST skip_deps=true eunit -r -v
console:
	erl -pa ebin deps/*/ebin
###
### Docs
###
docs:
	$(REBAR) skip_deps=true doc

##
## Developer targets
##

xref: all
	$(REBAR) xref skip_deps=true -r

##
## Dialyzer
##
APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
       xmerl webtool snmp public_key mnesia eunit syntax_tools compiler edoc

COMBO_PLT = $(HOME)/.fifo_api_combo_dialyzer_plt

# DIALYZER_IGNORE="^\(riak_core\|leexinc.hrl\|pokemon_pb.erl\|meck_cover.erl\|meck.erl\|supervisor_pre_r14b04.erl\|webmachine_resource.erl\|uuid.erl\|gen_server2.erl\|folsom_vm_metrics.erl\|protobuffs_compile.erl\)"

check_plt: deps compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin ebin

build_plt: deps compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin ebin

dialyzer: deps compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) deps/*/ebin ebin | grep -v -f dialyzer.mittigate

dialyzer-gui: deps compile
	dialyzer --gui -Wno_return --plt $(COMBO_PLT) deps/*/ebin ebin
typer:
	typer --plt $(COMBO_PLT) deps/*/ebin ebin

cleanplt:
	@echo
	@echo "Are you sure?  It takes about 1/2 hour to re-build."
	@echo Deleting $(COMBO_PLT) in 5 seconds.
	@echo
	sleep 5
	rm $(COMBO_PLT)

tags:
	find . -name "*.[he]rl" -print | etags -
