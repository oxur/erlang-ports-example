VERSION = $(shell cat VERSION)
DVCS_HOST = github.com
ORG = geomyidia
PROJ = erlang-port-example
FQ_PROJ = $(DVCS_HOST)/$(ORG)/$(PROJ)

LD_VERSION = -X $(FQ_PROJ)/internal/app.version=$(VERSION)
LD_BUILDDATE = -X $(FQ_PROJ)/internal/app.buildDate=$(shell date -u +"%Y-%m-%dT%H:%M:%SZ")
LD_GITCOMMIT = -X $(FQ_PROJ)/internal/app.gitCommit=$(shell git rev-parse --short HEAD)
LD_GITBRANCH = -X $(FQ_PROJ)/internal/app.gitBranch=$(shell git rev-parse --abbrev-ref HEAD)
LD_GITSUMMARY = -X $(FQ_PROJ)/internal/app.gitSummary=$(shell git describe --tags --dirty --always)
LDFLAGS = -w -s $(LD_VERSION) $(LD_BUILDDATE) $(LD_GITBRANCH) $(LD_GITSUMMARY) $(LD_GITCOMMIT)

default: all

all: deps bin/echo

bin:
	@mkdir ./bin

bin/echo: bin
	@GO111MODULE=on go build -ldflags "$(LDFLAGS)" -o bin/echo ./cmd/echo

run:
	@GO111MODULE=on go run ./cmd/echo

clean:
	@rm -f ./bin/*

deps:
	@go mod tidy
	@go get -u golang.org/x/sys
