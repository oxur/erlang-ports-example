PROJ = ports
REL_DIR = ./_build/default/rel/$(PROJ)
PROJ_BIN = $(REL_DIR)/bin/$(PROJ)
PRIV = ./apps/$(PROJ)/priv
PWD = $(shell pwd)

#############################################################################
###   General Targets   #####################################################
#############################################################################

default: build

build: build-cl build-go release

clean-all: clean clean-cl clean-go

.PHONY: default run release shutdown run-fresh build build-cl build-go clean-cl clean-go

#############################################################################
###   Erlang Targets   ######################################################
#############################################################################

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

GO_BASE = $(PRIV)/go/src/github.com/geomyidia
GO_PROJ = $(GO_BASE)/erlang-port-examples

$(GO_PROJ):
	@echo ">> Setting up Go examples ..."
	-@mkdir $(GO_BASE) && \
	  git clone https://github.com/geomyidia/erlang-port-examples.git $(GO_PROJ)

build-go: | $(GO_PROJ)
	@echo ">> Building Go examples ..."
	@cd $(GO_PROJ) && $(MAKE)

clean-go:
	@cd $(GO_PROJ) && $(MAKE) clean

#############################################################################
###   Common Lisp Targets   #################################################
#############################################################################

CL_PROJ = $(PRIV)/cl-port-examples

$(CL_PROJ):
	@echo ">> Setting up Common Lisp examples ..."
	@git clone https://github.com/cl-axon/erlang-port-examples.git $(CL_PROJ)

build-lisp: | $(CL_PROJ)
	@echo ">> Building Common Lisp examples ..."
	@cd $(CL_PROJ) && $(MAKE)

clean-lisp:
	@cd $(CL_PROJ) && $(MAKE) clean
