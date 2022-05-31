# bootstrap.R - DESC
# /home/mosqu003/Active/WORKSHOP_JRC_MSE-MAY2022/mse_fleets_WK/bootstrap.R

# Copyright (c) WUR, 2022.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(remotes)

# INSTALL from r-universe

install.packages(c("mse", "ggplotFL", "ss3om"),
  repos=c(FLR="https://flr.r-universe.dev",
  CRAN="https://cloud.r-project.org/"))

# INSTALL from github

install_github(paste0("flr",
  c("FLCore", "FLasher", "mse", "ggplotFL", "ss3om")))

