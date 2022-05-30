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


# --- APPLY CCSBT trend HCR

# GET allocation per fleet

totc <- unlist(lapply(iter(window(catch(fisheries(om)),
  start=2021, end=2021), 1), function(x) seasonSums(unitSums(x))))
alloc <- totc / sum(totc)

# control: perfect.sa + trend.hcr

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=perfect.sa),
  # CCSBT trend HCR
  hcr = mseCtrl(method=trend.hcr,
    args=list(k1=1, k2=3, gamma=0.80, nyears=5, metric=ssb)),
  #
  isys = mseCtrl(method=splitTAC.is, args=list(allocation=alloc))
))

# RUN mp

trend <- mp(om, oem=oem, ctrl=control, args=mseargs)

plot(om, trend)

# Same MP can be run every 3 years

trend3y <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, frq=3))

plot(om, trend3y)


# --- RUN mean length indicator + target level HCR

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=len.ind, args=list(indicator="mlc",
    params=FLPar(linf=35, k=0.352, t0=-0.26), cv=0.2)),
  # CCSBT trend HCR
  hcr = mseCtrl(method=target.hcr,
    args=list(lim=15, target=20, metric="mlc")),
  #
  isys = mseCtrl(method=splitTAC.is, args=list(allocation=alloc))
))

length <- mp(om, oem=oem, ctrl=control, args=mseargs)


# --- APPLY ICES HCR and TAC short-term forecast

# control: perfect.sa + ices.hcr + tac.is

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=perfect.sa),
  # ICES HCR
  hcr = mseCtrl(method=ices.hcr,
    args=list(ftrg=c(refpts(om)$FMSY) * 0.5, sblim=c(refpts(om)$SBlim),
      sbsafe=c(refpts(om)$Bpa))),
  # One-year forecast for TAC
  isys=mseCtrl(method=tac.is, args=list(dtaclow=0.85, dtacupp=1.15, recyrs=30)),
  fb = mseCtrl(method=splitTAC.is, args=list(allocation=alloc))
  ))

# RUN mp

ices <- mp(om, oem=oem, ctrl=control, args=mseargs)


# --- TUNE MP for 60% P(SB = SBMSY) over years 2030:2040

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=len.ind, args=list(indicator="mlc",
    params=FLPar(linf=35, k=0.352, t0=-0.26), cv=0.2)),
  # CCSBT trend HCR
  hcr = mseCtrl(method=target.hcr,
    args=list(lim=15, target=20, metric="mlc")),
  #
  isys = mseCtrl(method=splitTAC.is, args=list(allocation=alloc))
))

# LOAD performance statistics

data(statistics)

# SET metrics

mets <- list(SB=ssb)

# TUNE

tun <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets, statistic=statistics["PSBMSY"], years=2030:2040,
  tune=list(sblim=c(10000, 50000)), prob=0.6, tol=0.01, maxit=12)


# --- ASSEMBLE MP runs

runs <- list(TREND=trend, TREND2Y=trend3y, LEN=length, ICES=ices)

plot(om, runs)

save(runs, file="model/runs.Rdata", compress="xz")
