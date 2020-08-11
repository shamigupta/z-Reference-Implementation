#!/usr/bin/env Rscript
library(plumber)
pr1 <- plumb('/srv/shiny-server/myRapiv3.R')
pr1$run(port=8000, host="0.0.0.0")
# Setting the host option on a VM instance ensures the application can be accessed externally.
# (This may be only true for Linux users.)