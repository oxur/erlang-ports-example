PROJ = ports
REL_DIR = ./_build/default/rel/$(PROJ)
PROJ_BIN = $(REL_DIR)/bin/$(PROJ)
PRIV = apps/$(PROJ)/priv
PWD = $(shell pwd)

#############################################################################
###   General Targets   #####################################################
#############################################################################

default: build

build: $(PRIV) build-cl build-go release

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

#############################################################################
###   Erlang Targets   ######################################################
#############################################################################

$(PRIV):
	@mkdir -p $(PRIV)

$(PROJ_BIN):
	@echo '>> Building release ...'
	@rebar3 release

release: | $(PROJ_BIN)

run: release
	@echo '>> Running application from distribution console ...'
	@echo $(PROJ_BIN)
	@GO111MODULE=on GOPATH=$(PWD)/apps/ports/priv/go $(PROJ_BIN) console

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

GO_REPO = https://github.com/geomyidia/erlang-port-examples.git
GO_BASE = $(PRIV)/go/src/github.com/geomyidia
GO_PROJ = erlang-port-examples
GO_DIR = $(GO_BASE)/$(GO_PROJ)

$(GO_BASE):
	@mkdir -p $(GO_BASE)

init-go: $(GO_BASE)
	@echo ">> Setting up Go examples ..."
	@git subtree add \
	   --prefix $(GO_DIR) \
	   $(GO_REPO) \
	   master \
	   --squash

update-go:
	@echo ">> Updating Go examples ..."
	@git subtree pull \
	   --m "Updated latest from $(PRIV)/go." \
	   --prefix $(GO_DIR) \
	   $(GO_REPO) \
	   master \
	   --squash

build-go: | $(GO_DIR)
	@echo ">> Building Go examples ..."
	@cd $(GO_DIR) && $(MAKE)

clean-go:
	@cd $(GO_DIR) && $(MAKE) clean

#############################################################################
###   Common Lisp Targets   #################################################
#############################################################################

CL_REPO = https://github.com/cl-axon/erlang-port-examples.git
CL_PROJ = erlang-port-examples
CL_DIR = $(PRIV)/cl-port-examples

init-lisp:
	@echo ">> Setting up Common Lisp examples ..."
	@git subtree add \
	   --prefix $(CL_DIR) \
	   $(CL_REPO) \
	   master \
	   --squash

update-lisp:
	@echo ">> Updating Common Lisp examples ..."
	@git subtree pull \
	   --m "Updated latest from Lisp $(CL_DIR)." \
	   --prefix $(CL_DIR) \
	   $(CL_REPO) \
	   master \
	   --squash

build-cl: | $(CL_DIR)
	@echo ">> Building Common Lisp examples ..."
	@cd $(CL_DIR) && $(MAKE)

clean-cl:
	@cd $(CL_DIR) && $(MAKE) clean

#############################################################################
###   All Languages   #######################################################
#############################################################################

init: \
	init-go \
	init-lisp

update: \
	update-go \
	update-lisp
