package port

import (
	"math/rand"
	"time"

	"github.com/geomyidia/zylog/logger"
)

// SetupApp ...
func SetupApp() {
	logger.SetupLogging(&logger.ZyLogOptions{
		Colored:      false,
		Level:        "fatal",
		Output:       "stderr",
		ReportCaller: true,
	})
	rand.Seed(time.Now().Unix())
}
