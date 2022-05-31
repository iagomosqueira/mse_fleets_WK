# model.R - Running mse with NS sol.27.40
# FLom/model.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)

source("utilities.R")

load('data/flom.Rdata')

# SET intermediate year, start of runs

mseargs <- list(iy=2021)

# SET parallel

library(doParallel)
registerDoParallel(4)

# windows
cl <- makeCluster(4, type="PSOCK")  
registerDoParallel(cl)  


# --- RUN perfect.sa + hockeystick.hcr

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=perfect.sa),
  # hockey-stick (catch ~ ssb)
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=3000, trigger=12000, target=24000, min=1000,
      metric="ssb", output="catch"))
))

plot_hockeystick.hcr(control$hcr)

hckstk <- mp(om, oem=oem, ctrl=control, args=mseargs)

hckstk3 <- mp(om, oem=oem, ctrl=control, args=list(iy=2017, frq=3))

plot(om, HS=hckstk, HS3=hckstk3, metrics=mets)


# --- RUN mean length indicator + target level HCR

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=len.ind, args=list(indicator="mlc",
    params=FLPar(linf=132, k=0.080, t0=-0.35), cv=0.2)),
  # CCSBT trend HCR
  hcr = mseCtrl(method=target.hcr,
    args=list(lim=60, target=100, metric="mlc"))
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
  metrics=mets, statistic=statistics["PSBMSY"], years=2030:2035,
  tune=list(target=c(60, 120)), prob=0.6, tol=0.01, maxit=12)


# --- ASSEMBLE MP runs

runs <- list(TREND=trend, TREND2Y=trend3y, LEN=length, ICES=ices)

plot(om, runs)

save(runs, file="model/runs.Rdata", compress="xz")
