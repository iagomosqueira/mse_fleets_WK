# model.R - Running mse with NS sol.27.40
# FLom/model.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)

load('data/om.Rdata')

# SET intermediate year, start of runs

mseargs <- list(iy=2021)

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
    args=list(lim=12000, trigger=20000, target=2.41e4, min=100,
      metric="ssb", output="catch")),
  #
  isys = mseCtrl(method=splitTAC.is, args=list(allocation=alloc))
))

# BUG:
hckstk <- mp(om, oem=oem, ctrl=control, args=mseargs)


# --- RUN xsa.sa + hockeystick.hcr

library(FLXSA)

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=xsa.sa),
  # hockey-stick (catch ~ ssb)
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=12000, trigger=20000, target=2.41e4, min=100,
      metric="ssb", output="catch")),
  #
  isys = mseCtrl(method=splitTAC.is, args=list(allocation=alloc))
))

# BUG:
xsamp <- mp(om, oem=oem, ctrl=control, args=mseargs)




# --- RUN mean length indicator + target level HCR

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=len.ind, args=list(indicator="mlc",
    params=FLPar(linf=132, k=0.080, t0=-0.35), cv=0.2)),
  # CCSBT trend HCR
  hcr = mseCtrl(method=target.hcr,
    args=list(lim=60, target=100, metric="mlc")),
  #
  isys = mseCtrl(method=splitTAC.is, args=list(allocation=alloc))
))

length <- mp(om, oem=oem, ctrl=control, args=mseargs)

length3 <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, frq=3))


# --- TUNE MP for 60% P(SB = SBMSY) over years 2030:2040

# LOAD performance statistics

data(statistics)

# SET metrics

mets <- list(SB=ssb)

# TUNE

tun <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets, statistic=statistics["PSBMSY"], years=2030:2040,
  tune=list(target=c(60, 120)), prob=0.6, tol=0.01, maxit=12)


# --- ASSEMBLE MP runs

runs <- list(TREND=trend, TREND2Y=trend3y, LEN=length, ICES=ices)

plot(om, runs)

save(runs, file="model/runs.Rdata", compress="xz")
