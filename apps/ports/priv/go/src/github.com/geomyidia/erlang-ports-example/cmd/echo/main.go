package main

import (
	"context"
	"sync"
	"syscall"

	log "github.com/sirupsen/logrus"

	"github.com/geomyidia/erlcmd/pkg/messages"
	"github.com/geomyidia/erlcmd/pkg/options"
	"github.com/geomyidia/erlcmd/pkg/packets"

	"github.com/geomyidia/erlang-ports-example/internal/app"
	"github.com/geomyidia/erlang-ports-example/pkg/echo"
)

func main() {
	app.SetupLogging()
	log.Info("Starting up Go Port example ...")
	log.Infof("Running version: %s", app.VersionedBuildString())
	app.SetupRandom()
	ctx, cancel := app.SignalWithContext(context.Background(), syscall.SIGINT, syscall.SIGTERM)
	defer cancel()
	var wg sync.WaitGroup

	wg.Add(1)
	go func() {
		defer wg.Done()
		HandleMessages(ctx)
	}()

	// Listen for the interrupt signal.
	<-ctx.Done()

	// Restore default behavior on the interrupt signal and notify user of shutdown.
	cancel()
	log.Info("Shutting down gracefully, press Ctrl+C again to force")
	log.Info("Waiting for wait groups to finish ...")
	wg.Wait()
	log.Info("Application shutdown complete.")
}

func HandleMessages(ctx context.Context) {
	log.Info("processing messages sent to Go language server ...")
	go func() {
		for {
			HandleMessage()
			continue
		}
	}()
	<-ctx.Done()
}

func HandleMessage() {
	// term, err := packets.ToTerm(&options.Opts{IsHexEncoded: true})
	term, err := packets.ToTerm(options.DefaultOpts())
	if err != nil {
		echo.SendError(err)
		return
	}
	log.Tracef("got Erlang ports term: %#v", term)
	msg, err := messages.New(term, options.DefaultOpts())
	if err != nil {
		echo.SendError(err)
		return
	}

	msgName := msg.Name()
	log.Tracef("Got message name %s", msgName)
	switch msg.Type() {
	case string(messages.CommandKey):
		echo.Dispatch(msgName)
	default:
		echo.SendError(echo.ErrUnsupCmd)
	}
	log.Trace("message handling complete")
}
