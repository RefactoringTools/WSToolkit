# Copyright 2012 Erlware, LLC. All Rights Reserved.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#

ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin

REBAR_REPO := https://github.com/rebar/rebar3.git
# REBAR_VERSION := 3.0.0
REBAR_REPO_DIR := rebar
REBAR := $(REBAR_REPO_DIR)/rebar

DEPS_PLT=$(CURDIR)/.deps_plt
DEPS=erts kernel stdlib

# =============================================================================
# Verify that the programs we need to run are installed on this system
# =============================================================================
ERL = $(shell which erl)

ifeq ($(ERL),)
$(error "Erlang not available on this system")
endif

$(REBAR): $(REBAR_REPO_DIR)
	cd $(REBAR_REPO_DIR); ./bootstrap

$(REBAR_REPO_DIR):
	git clone $(REBAR_REPO) $(REBAR_REPO_DIR)
	cd $(REBAR_REPO_DIR); git checkout -q $(REBAR_VERSION)

.PHONY: all compile doc clean test dialyzer typer shell distclean pdf \
  update-deps clean-common-test-data rebuild

all: deps compile dialyzer test

# =============================================================================
# Rules to build the system
# =============================================================================

deps: $(REBAR)
	$(REBAR) get-deps
	$(REBAR) compile

update-deps:
	$(REBAR) update-deps
	$(REBAR) compile

compile:
	$(REBAR) skip_deps=true compile

doc:
	$(REBAR) skip_deps=true doc

test: compile test-without-cover test-with-cover

test-with-cover: compile
	COVER_ENABLED=true $(REBAR) eunit skip_deps=true

test-without-cover: compile
	$(REBAR) eunit skip_deps=true

check: xref dialyzer

xref: compile
	$(REBAR) xref skip_deps=true

$(DEPS_PLT):
	@echo Building local plt at $(DEPS_PLT)
	@echo
	dialyzer --output_plt $(DEPS_PLT) --build_plt \
	   --apps $(DEPS) -r deps

dialyzer: $(DEPS_PLT)
	dialyzer --fullpath --plt $(DEPS_PLT) -Wrace_conditions -r ./ebin

typer:
	typer --plt $(DEPS_PLT) -r ./src

shell: deps compile
# You often want *rebuilt* rebar tests to be available to the
# shell you have to call eunit (to get the tests
# rebuilt). However, eunit runs the tests, which probably
# fails (thats probably why You want them in the shell). This
# runs eunit but tells make to ignore the result.
	- @$(REBAR) skip_deps=true eunit
	@$(ERL) $(ERLFLAGS)

pdf:
	pandoc README.md -o README.pdf

clean:
	- rm -rf $(CURDIR)/test/*.beam
	- rm -rf $(CURDIR)/logs
	- rm -rf $(CURDIR)/ebin
	$(REBAR) skip_deps=true clean

distclean: clean
	- rm -rf $(DEPS_PLT)
	- rm -rf $(CURDIR)/deps
	- rm -rf $(REBAR_REPO_DIR)

rebuild: distclean deps compile dialyzer test
