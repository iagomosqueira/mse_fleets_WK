# test.R - DESC
# /test.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)

library(patchwork)

# -- data_flom.R

load("data/flom.Rdata")

# hindcast for 1970:2021

tes <- fwd(stock(om), sr=sr(om),
  catch=unitSums(catch(stock(om))[, ac(1970:2021)]),
  deviances=residuals(sr(om)))

plot(stock(om), tes, metrics=mets)

ctrl <- as(FLQuants(catch=unitSums(catch(stk)[, ac(1980:2021)])), 'fwdControl')

# BUG: fwd to use deviances @ residuals
tes <- fwd(com, control=ctrl,
  deviances=expand(residuals(srr), unit=c('F', 'M')))


# --- model_flombf.R

load("data/flombf.Rdata")


ctrl <- fwdControl(
lapply(1:4, function(x)
  list(year=2022:2040, fishery=x, catch=1, quant="catch",
    relYear=2021:2039, relFishery=x, relCatch=1, value=1))
)

hin <- fwd(om, control=ctrl)

plot(hin)

plot(window(hin) / plot(om)

ctrl <- fwdControl(
  list(year=2000:2021, biol=1, quant="fbar", value=0, minAge=2, maxAge=12),
  list(year=2000:2021, fishery=2, catch=1, quant="catch", value=0),
  list(year=2000:2021, fishery=3, catch=1, quant="catch", value=0),
  list(year=2000:2021, fishery=4, catch=1, quant="catch", value=0)
)

f0 <- fwd(om, control=ctrl)


plot(hin) / plot(om)
plot(hin) / plot(f0)

catch(fisheries(hin)) / catch(fisheries(om))
