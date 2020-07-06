PROJ = ports
REL_DIR = ./_build/default/rel/$(PROJ)
PROJ_BIN = $(REL_DIR)/bin/$(PROJ)
PRIV = ./apps/$(PROJ)/priv
GO_BASE = $(PRIV)/go/src/github.com/geomyidia
GO_PROJ = $(GO_BASE)/erlang-port-examples
SUB_PROJS = $(GO_PROJ)
PWD = $(shell pwd)

default: build

build: build-go release

$(PROJ_BIN):
	@echo '>> Building release ...'
	@rebar3 release

release: | $(PROJ_BIN)

$(GO_PROJ):
	@echo ">> Setting up Go examples ..."
	@mkdir -p $(GO_BASE)
	@git clone https://github.com/geomyidia/erlang-port-examples.git $(GO_PROJ)

build-go: | $(GO_PROJ)
	@cd $(GO_PROJ) && $(MAKE)

run: release
	@echo '>> Running application from distribution console ...'
	@echo $(PROJ_BIN)
	@GO111MODULE=on GOPATH=$(PWD)/apps/ports/priv/go $(PROJ_BIN) console

shutdown:
	@echo '>> Shutting down OTP application ...'
	-@$(PROJ_BIN) status || $(PROJ_BIN) stop
	@ps aux|grep $(PROJ_BIN)|grep -v grep

clean:
	@rebar3 clean
	@rm -rf $(REL_DIR)

clean-all: clean
	@cd $(GO_PROJ) && $(MAKE) clean

.PHONY: default run release $(SUB_PROJS)
