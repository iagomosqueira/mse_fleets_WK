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

plot(om, HS=hckstk, metrics=mets)

hckstk3 <- mp(om, oem=oem, ctrl=control, args=list(iy=2017, frq=3))

plot(om, HS=hckstk, HS3=hckstk3, metrics=mets)


# --- RUN xsa.sa + hockeystick.hcr

library(FLXSA)

om <- fwd(om, control=fwdControl(
  year=2010:2021, quant="fbar", biol=1, value=0.4, minAge=3, maxAge=15))


control <- mpCtrl(list(
  # xsa.sa
  est = mseCtrl(method=xsa.sa,
    args=list(control=FLXSA.control(shk.n=FALSE, vpa=TRUE, qage=5))),
  # hockey-stick (catch ~ ssb)
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=3000, trigger=12000, target=24000, min=1000,
      metric="ssb", output="catch"))
))

xsamp <- mp(om, oem=oem, ctrl=control, args=mseargs)

plot(om, XSA=xsamp, metrics=mets)


# --- RUN sca.sa + hockeystick.hcr

library(FLa4a)

control <- mpCtrl(list(
  # sca.sa
  est = mseCtrl(method=sca.sa, args=list()),
  # hockey-stick (catch ~ ssb)
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=3000, trigger=12000, target=24000, min=1000,
      metric="ssb", output="catch"))
))

scamp <- mp(om, oem=oem, ctrl=control, args=mseargs)

plot(om, XSA=xsamp, metrics=mets)


# --- RUN mean length indicator + target level HCR

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=len.ind, args=list(indicator="mlc",
    params=FLPar(linf=132, k=0.080, t0=-0.35), cv=0.2)),
  # target HCR
  hcr = mseCtrl(method=target.hcr,
    args=list(lim=20, target=50, metric="mlc"))
))

length <- mp(om, oem=oem, ctrl=control, args=mseargs)

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=len.ind, args=list(indicator="mlc",
    params=FLPar(linf=150, k=0.120, t0=-0.35), cv=0.2,
    # MATURE length
    metric=function(stk) catch.n(stk) * mat(stk))),
  # target HCR
  hcr = mseCtrl(method=target.hcr,
    args=list(lim=40, target=80, metric="mlc"))
))

length <- mp(om, oem=oem, ctrl=control, args=mseargs)

plot(om, LEMMAT=length, metrics=mets)


# --- RUNS

plot(om, HSTK=hckstk, XSA=xsamp, LEN=length, metrics=mets)

runs <- list(HSTK=hckstk, HSTK3=hckstkk3, XSA=xsamp, LEN=length)

save(om, runs, file="model/flomruns.Rdata", compress="xz")
