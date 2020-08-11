#!/usr/bin/env Rscript
library(plumber)
pr2 <- plumb('/srv/shiny-server/myRapiv4.R')
pr2$run(port=8100, host="0.0.0.0")
# Setting the host option on a VM instance ensures the application can be accessed externally.
# (This may be only true for Linux users.)