# 00 Preparation ---------------------------------------------------------------
cat("System information:\n")
for (i in seq_along(sysinfo <- Sys.info()))
  cat("  ", names(sysinfo)[i], ":", sysinfo[i], "\n")

library("PlotR")
library("methods")

sessionInfo()

startApplication(port = 3838)
