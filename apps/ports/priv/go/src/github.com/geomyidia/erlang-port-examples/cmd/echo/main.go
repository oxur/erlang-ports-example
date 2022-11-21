package main

import (
	"context"
	"sync"
	"syscall"

	log "github.com/sirupsen/logrus"

	"github.com/geomyidia/erlang-port-examples/internal/app"
	"github.com/geomyidia/erlang-port-examples/pkg/echo"
	"github.com/geomyidia/erlang-port-examples/pkg/port"
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
		port.ProcessPortMessages(ctx, echo.ProcessEchoCommand)
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
