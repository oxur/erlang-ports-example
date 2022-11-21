default: all

all: deps bin/echo

bin:
	@mkdir ./bin

bin/echo: bin
	@GO111MODULE=on go build -o bin/echo ./cmd/echo

run:
	@GO111MODULE=on go run ./cmd/echo

clean:
	@rm -f ./bin/*

deps:
	@go get -u golang.org/x/sys
