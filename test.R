# test.R - DESC
# /test.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# -- data.R

# HINDCAST om

# --- TESTS


ctrl <- fwdControl(
lapply(1:4, function(x)
  list(year=2000:2021, fishery=x, catch=1, quant="catch",
    value=c(unitSums(catch(fisheries(om)[[x]][[1]]))[, ac(2000:2021)])))
)

hin <- fwd(om, control=ctrl)

plot(hin) / plot(om)

ctrl <- fwdControl(
  list(year=2000:2021, biol=1, quant="fbar", value=0, minAge=2, maxAge=12),
  list(year=2000:2021, fishery=2, catch=1, quant="catch", value=0),
  list(year=2000:2021, fishery=3, catch=1, quant="catch", value=0),
  list(year=2000:2021, fishery=4, catch=1, quant="catch", value=0)
)

f0 <- fwd(om, control=ctrl)


library(patchwork)

plot(hin) / plot(om)
plot(hin) / plot(f0)

catch(fisheries(hin)) / catch(fisheries(om))


# SIMPLIFY om (unit, fleets)


# CREATE 2 surveys
