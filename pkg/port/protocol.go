package port

import (
	"bufio"
	"context"
	"fmt"
	"os"

	erlang "github.com/okeuday/erlang_go/src/erlang"
	log "github.com/sirupsen/logrus"
)

// Constants
const (
	READERSIZE    = 1677216
	DELIMITER     = '\n'
	CMDARITY      = 2
	CMDKEYINDEX   = 0
	CMDVALUEINDEX = 1
)

// CommandProcessor ...
type CommandProcessor func(string)

// ProcessPortMessages handles messages of the Erlang Port format along the
// following lines:
//   a           = []byte{0x83, 0x64, 0x0, 0x1, 0x61, 0xa}
//   "a"         = []byte{0x83, 0x6b, 0x0, 0x1, 0x61, 0xa}
//   {}          = []byte{0x83, 0x68, 0x0, 0xa}
//   {a}         = []byte{0x83, 0x68, 0x1, 0x64, 0x0, 0x1, 0x61, 0xa}
//   {"a"}       = []byte{0x83, 0x68, 0x1, 0x6b, 0x0, 0x1, 0x61, 0xa}
//   {a, a}      = []byte{0x83, 0x68, 0x2, 0x64, 0x0, 0x1, 0x61, 0x64, 0x0, 0x1, 0x61, 0xa}
//   {a, test}   = []byte{0x83, 0x68, 0x2, 0x64, 0x0, 0x1, 0x61, 0x64, 0x0, 0x4, 0x74, 0x65, 0x73, 0x74, 0xa}
//   {a, "test"} = []byte{0x83, 0x68, 0x2, 0x64, 0x0, 0x1, 0x61, 0x6b, 0x0, 0x4, 0x74, 0x65, 0x73, 0x74, 0xa}
func ProcessPortMessages(ctx context.Context, fn CommandProcessor) {
	log.Info("Processing messages sent to Go language server ...")
	go func() {
		for {
			cmd := ProcessPortMessage()
			if cmd == "continue" {
				continue
			}
			fn(cmd)

		}
	}()
	<-ctx.Done()
}

// ProcessPortMessage ...
func ProcessPortMessage() string {
	var term interface{}
	reader := bufio.NewReaderSize(os.Stdin, READERSIZE)
	packet, _ := reader.ReadBytes(DELIMITER)
	if len(packet) == 0 {
		log.Fatal("Read zero bytes")
		return "continue"
	}
	log.Debugf("Original packet: %#v", packet)
	log.Debugf("Packet length: %d", len(packet))
	// Drop the message's field separator, \0xa (newline)
	packet = packet[:len(packet)-1]
	log.Debugf("New packet: %#v", packet)
	term, err := erlang.BinaryToTerm(packet)
	if err != nil {
		log.Errorf("Problem with packet: %#v", packet)
		log.Error(err)
		return "continue"
	}
	log.Debugf("Got Erlang Port message: %#v", term)
	tuple, ok := term.(erlang.OtpErlangTuple)
	if !ok {
		SendError("Did not receive expected message type")
		return "continue"
	}
	if len(tuple) != CMDARITY {
		SendError(fmt.Sprintf("Tuple of wrong size; expected 2, got %d", len(tuple)))
		return "continue"
	}
	if tuple[CMDKEYINDEX] != erlang.OtpErlangAtom("command") {
		SendError("Did not receive expected tuple message format {command, ...}")
		return "continue"
	}
	command, ok := tuple[CMDVALUEINDEX].(erlang.OtpErlangAtom)
	if !ok {
		SendError("Did not receive command as Erlang atom")
		return "continue"
	}
	return string(command)
}
