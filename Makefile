default: all

all: bin/echo

bin:
	@mkdir ./bin

bin/echo: bin
	@GO111MODULE=on go build -o bin/echo ./cmd/echo

run:
	@GO111MODULE=on go run ./cmd/echo

clean:
	@rm -f ./bin/*
