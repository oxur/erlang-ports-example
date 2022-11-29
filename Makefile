PROJ = ports
REL_DIR = ./_build/default/rel/$(PROJ)
PROJ_BIN = $(REL_DIR)/bin/$(PROJ)
PRIV = apps/$(PROJ)/priv
PWD = $(shell pwd)

#############################################################################
###   General Targets   #####################################################
#############################################################################

default: build

build: build-cl build-go build-rust release

clean-all: clean clean-cl clean-go clean-rust

.PHONY: default run release shutdown run-fresh build build-cl build-go build-rust clean-cl clean-go clean-rust

help:
	@echo
	@echo "To add a new language to this repo, look at the Makefile targets"
	@echo "for the other languages already present. Create targets like"
	@echo "those (along with a unique VAR for the languages's repo and"
	@echo "directory)."
	@echo
	@echo "Once you've added your targets, run your 'init' target via"
	@echo "make. Future updates to your language repo can be pulled in here"
	@echo "with your added 'update' target (or you can update all repos with"
	@echo "the general-purpose update target)."
	@echo

init: \
	init-go \
	init-lisp \
	init-rust

push: \
	push-go \
	push-lisp \
	push-rust

pull: \
	pull-go \
	pull-lisp \
	pull-rust

#############################################################################
###   Erlang Targets   ######################################################
#############################################################################

$(PROJ_BIN):
	@echo '>> Building release ...'
	@rebar3 lfe compile
	@rebar3 release

release: | $(PROJ_BIN)

run: release
	@echo '>> Running application from distribution console ...'
	@echo $(PROJ_BIN)
	@ERL_AFLAGS="-kernel shell_history enabled" \
	GO111MODULE=on GOPATH=$(PWD)/apps/ports/priv/go \
	$(PROJ_BIN) console

run-fresh: clean-all build run

shutdown:
	@echo '>> Shutting down OTP application ...'
	-@$(PROJ_BIN) status || $(PROJ_BIN) stop
	@ps aux|grep $(PROJ_BIN)|grep -v grep

clean:
	@rebar3 clean
	@rm -rf $(REL_DIR)

#############################################################################
###   Rust Targets   ########################################################
#############################################################################

RUST_REPO = https://github.com/oxur/erlang-ports-example.git
RUST_BASE = $(PRIV)
RUST_PROJ = erlang-ports-example
RUST_DIR = $(RUST_BASE)/ports-example-rs

$(RUST_BASE):
	@mkdir -p $(RUST_BASE)

init-rust: $(RUST_BASE)
	@echo ">> Setting up Rust example ..."
	-@git subtree add \
	   --prefix $(RUST_DIR) \
	   $(RUST_REPO) \
	   main \
	   --squash

pull-rust:
	@echo ">> Updating local Rust example from origin ..."
	@git subtree pull \
	   --m "Updated latest from LFE examples ($(PRIV))." \
	   --prefix $(RUST_DIR) \
	   $(RUST_REPO) \
	   main \
	   --squash

push-rust:
	@echo ">> Updating remote Rust example from local ..."
	@git subtree push \
	   --prefix $(RUST_DIR) \
	   $(RUST_REPO) \
	   main

build-rust: | $(RUST_DIR)
	@echo ">> Building Rust example ..."
	@cd $(RUST_DIR) && $(MAKE)

clean-rust:
	@cd $(RUST_DIR) && $(MAKE) clean

#############################################################################
###   Go Targets   ##########################################################
#############################################################################

GO_REPO = https://github.com/geomyidia/erlang-ports-example.git
GO_BASE = $(PRIV)/go/src/github.com/geomyidia
GO_PROJ = erlang-ports-example
GO_DIR = $(GO_BASE)/$(GO_PROJ)

$(GO_BASE):
	@mkdir -p $(GO_BASE)

init-go: $(GO_BASE)
	@echo ">> Setting up Go example ..."
	-@git subtree add \
	   --prefix $(GO_DIR) \
	   $(GO_REPO) \
	   main \
	   --squash

pull-go:
	@echo ">> Updating Go example ..."
	@git subtree pull \
	   --m "Updated latest from $(PRIV)/go." \
	   --prefix $(GO_DIR) \
	   $(GO_REPO) \
	   main \
	   --squash

push-go:
	@echo ">> Updating Go example ..."
	@git subtree push \
	   --prefix $(GO_DIR) \
	   $(GO_REPO) \
	   main

build-go: | $(GO_DIR)
	@echo ">> Building Go example ..."
	@cd $(GO_DIR) && $(MAKE)

clean-go:
	@cd $(GO_DIR) && $(MAKE) clean

clean-build-go: clean-go build-go

#############################################################################
###   Common Lisp Targets   #################################################
#############################################################################

CL_REPO = https://github.com/cl-axon/erlang-ports-example.git
CL_PROJ = erlang-ports-example
CL_DIR = $(PRIV)/cl-ports-example

init-lisp:
	@echo ">> Setting up Common Lisp example ..."
	-@git subtree add \
	   --prefix $(CL_DIR) \
	   $(CL_REPO) \
	   main \
	   --squash

pull-lisp:
	@echo ">> Updating Common Lisp example ..."
	@git subtree pull \
	   --m "Updated latest from Lisp $(CL_DIR)." \
	   --prefix $(CL_DIR) \
	   $(CL_REPO) \
	   main \
	   --squash

push-lisp:
	@echo ">> Updating Common Lisp example ..."
	@git subtree push \
	   --prefix $(CL_DIR) \
	   $(CL_REPO) \
	   main

fixup-lisp:
	@echo ">> Fixing-up Common Lisp example ..."
	@git merge -s subtree -Xsubtree="$(CL_DIR)" origin/main --allow-unrelated-histories
	@git pull origin master --rebase -s subtree -Xsubtree="$(CL_DIR)" origin/main --allow-unrelated-histories

build-cl: | $(CL_DIR)
	@echo ">> Building Common Lisp example ..."
	@cd $(CL_DIR) && $(MAKE)

clean-cl:
	@cd $(CL_DIR) && $(MAKE) clean

quicklisp-link:
	@echo ">> Linking Common Lisp example to local Quicklisp ..."
	@cd apps/ports/priv/cl-ports-example/
	@ln -s `pwd` ~/quicklisp/local-projects/
