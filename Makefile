PROJ = ports
REL_DIR = ./_build/default/rel/$(PROJ)
PROJ_BIN = $(REL_DIR)/bin/$(PROJ)
PRIV = apps/$(PROJ)/priv
PWD = $(shell pwd)

#############################################################################
###   General Targets   #####################################################
#############################################################################

default: build

build: build-cl build-go release

clean-all: clean clean-cl clean-go

.PHONY: default run release shutdown run-fresh build build-cl build-go clean-cl clean-go

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
	init-lisp

push: \
	push-go \
	push-lisp

pull: \
	pull-go \
	pull-lisp

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
###   Go Targets   ##########################################################
#############################################################################

GO_REPO = https://github.com/geomyidia/erlang-port-example.git
GO_BASE = $(PRIV)/go/src/github.com/geomyidia
GO_PROJ = erlang-port-example
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

#############################################################################
###   Common Lisp Targets   #################################################
#############################################################################

CL_REPO = https://github.com/cl-axon/erlang-port-example.git
CL_PROJ = erlang-port-example
CL_DIR = $(PRIV)/cl-port-example

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

build-cl: | $(CL_DIR)
	@echo ">> Building Common Lisp example ..."
	@cd $(CL_DIR) && $(MAKE)

clean-cl:
	@cd $(CL_DIR) && $(MAKE) clean

quicklisp-link:
	@echo ">> Linking Common Lisp example to local Quicklisp ..."
	@cd apps/ports/priv/cl-port-example/
	@ln -s `pwd` ~/quicklisp/local-projects/
