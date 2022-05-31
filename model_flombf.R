# model.R - Running mse with NS sol.27.40
# FLom/model.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)

source("utilities.R")

load('data/flombf.Rdata')

# SET intermediate year, start of runs

mseargs <- list(iy=2021, fy=2039)

# SET parallel

library(doParallel)
registerDoParallel(4)

# windows
cl <- makeCluster(4, type="PSOCK")  
registerDoParallel(cl)  

# GET allocation per fleet

totc <- unlist(lapply(iter(window(catch(fisheries(om)),
  start=2021, end=2021), 1), function(x) seasonSums(unitSums(x))))
alloc <- totc / sum(totc)


# --- RUN perfect.sa + hockeystick.hcr

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=perfect.sa),
  # hockey-stick (catch ~ ssb)
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=3000, trigger=12000, target=24000, min=1000,
      metric="ssb", output="catch")),
  # fleet allocation
  isys = mseCtrl(method=splitTAC.is, args=list(allocation=alloc))
))

plot_hockeystick.hcr(control$hcr)

hckstk <- mp(om, oem=oem, ctrl=control, args=mseargs)

plot(om, HS=hckstk)


# --- RUN perfect.sa + hockeystick.hcr

library(FLXSA)

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=xsa.sa),
  # hockey-stick (catch ~ ssb)
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=3000, trigger=12000, target=24000, min=1000,
      metric="ssb", output="catch")),
  # fleet allocation
  isys = mseCtrl(method=splitTAC.is, args=list(allocation=alloc))
))

xsamp <- mp(om, oem=oem, ctrl=control, args=mseargs)



save(runs, file="model/runsbf.Rdata", compress="xz")
