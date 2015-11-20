REBAR = $(shell pwd)/rebar3
ELVIS = $(shell pwd)/elvis


compile: $(REBAR) .git/hooks/pre-commit
	$(REBAR) compile

.git/hooks/pre-commit: hooks/pre-commit
	cp hooks/pre-commit .git/hooks

pre-commit: lint xref dialyzer test

dialyzer: $(REBAR)
	$(REBAR) dialyzer

xref: $(REBAR)
	$(REBAR) xref

test: $(REBAR)
	$(REBAR) eunit

lint: $(ELVIS)
	$(ELVIS) rock

$(ELVIS):
	cp `which elvis` $(ELVIS)

$(REBAR):
	cp `which rebar3` $(REBAR)

tree: $(REBAR)
	$(REBAR) tree | grep -v '=' | sed 's/ (.*//' > tree

###
### Docs
###
docs:
	$(REBAR) edoc
