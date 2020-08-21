package echo

import (
	"os"

	"github.com/geomyidia/erlang-port-examples/port"
	log "github.com/sirupsen/logrus"
)

// ProcessEchoCommand ...
func ProcessEchoCommand(command string) {
	switch command {
	case "echo":
		port.SendResult("echo")
	case "stop":
		log.Info("Stopping Go Echo server ...")
		os.Exit(0)
	case "crash_it":
		os.Exit(1)
	default:
		port.SendError("Received unsupported command")
	}
}
