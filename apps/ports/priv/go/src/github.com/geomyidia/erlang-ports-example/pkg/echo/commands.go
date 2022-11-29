package echo

import (
	"errors"
	"os"

	log "github.com/sirupsen/logrus"
)

const (
	echo    = "echo"
	stop    = "stop"
	crashIt = "crash_it"
)

var (
	ErrUnsupCmd = errors.New("received unsupported command")
)

func Dispatch(command string) {
	switch command {
	case echo:
		SendResult("echo")
	case stop:
		log.Info("Stopping Go Echo server ...")
		os.Exit(0)
	case crashIt:
		os.Exit(1)
	default:
		SendError(ErrUnsupCmd)
	}
}
