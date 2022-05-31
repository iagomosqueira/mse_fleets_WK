# data_ang.R - DESC
# /home/mosquia/Active/WORKSHOP_JRC_MSE-MAY2022/data_ang.R

# Copyright (c) WUR, 2022.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)
library(ss3om)

source("utilities.R")

# SET final year & iters

fy <- 2040
nit <- 20

# LOAD anglerfish SS3 run

ang <- readOutputss3('data/ang/')

# --- BUILD om

# BUILD objects

stk <- readFLSss3('data/ang/', wtatage=TRUE)

srr <- buildFLSRss3(ang)
residuals(srr) <- expand(residuals(srr), unit=c('F', 'M'))

rps <- buildFLRPss330(ang)

# CONSTRUCT conditioned om

com <- FLom(stock=stk, projection=mseCtrl(method=fwd.om), sr=srr, refpts=rps)

plot(com, metrics=mets)

# EXTEND and propagate

om <- propagate(fwdWindow(com, end=fy), nit)

residuals(sr(om)) <- window(residuals(sr(om)), end=2040)
residuals(sr(om))[, ac(2022:2040)] <- residuals(sr(om))[, ac(sample(2012:2021, 19, replace=TRUE))]


# --- BUILD oem

# LOAD FLStock with no sexes

stk <- simplify(stock(om), 'unit')

# CREATE two surveys

idx <- buildFLIBss330(ang)

# indices

sur <- FLIndices(
  Q1=FLIndex(
    sel.pattern=sel.pattern(idx[[1]])[,,1],
    range=c(startf=0.15, endf=0.15)),
  Q3=FLIndex(
    sel.pattern=sel.pattern(idx[[2]])[,,1],
    range=c(startf=0.75, endf=0.75))
)

dimnames(sur[[1]])<-list(unit='unique', season='all')
dimnames(sur[[2]])<-list(unit='unique', season='all')

fit <- FLQuants(
  Q1=(stock.n(stk) * exp(-z(stk) * 0.15))[, ac(2003:2021)] *
    unitMeans(sel.pattern(idx[[1]])),
  Q3=(stock.n(stk) * exp(-z(stk) * 0.75))[, ac(2006:2021)] *
    unitMeans(sel.pattern(idx[[2]])))

iqs <- computeQ(sur, stk, fit=fit)

# BUG: 2021
iqs[[1]][,'2021'] <- iqs[[1]][,'2020']
iqs[[2]][,'2021'] <- iqs[[2]][,'2020']

index.q(sur[[1]]) <- iqs[[1]]
index.q(sur[[2]]) <- iqs[[2]]

sur[[1]] <- survey(stk, sur[[1]])
sur[[2]] <- survey(stk, sur[[2]])

sur <- fwdWindow(sur, end=2040)

sur <- FLIndices(Map(function(x, y){
  x@index = x@index * rlnorm(nit, x@index%=%0, y)
  x
  }, x=sur, y=c(0.23,0.27)) )

oem <- propagate(FLoem(method=sampling.oem,
  observations=list(stk=stk, idx=sur),
  deviances=list(
    stk=FLQuants(
      catch.n=rlnorm(20, catch.n(stk) %=% 0, 0.2)),
    idx=FLQuants(Q1=rlnorm(20, index.q(sur[[1]]) %=% 0, 0.1),
      Q2=rlnorm(20, index.q(sur[[2]]) %=% 0, 0.1)))),
  nit)

# SAVE

save(om, oem, file="data/flom.Rdata", compress="xz")
