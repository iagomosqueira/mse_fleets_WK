# output.R - OBTAIN performance statistics
# /output.R

# Copyright Iago MOSQUEIRA (WMR), 2022
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)

# LOAD OM
load("data/sol2740.Rdata")

# LOAD MP runs
load("model/runs.Rdata")

# SELECT statistics

data(statistics)

stats <- statistics[c("SBMSY", "FMSY", "risk1", "risk2")]

# and metrics

mets <- list(SB=ssb, F=fbar)

# COMPUTE performance over two year periods

perf <- performance(runs, statistics=stats, metrics=mets,
  years=list(2021:2025, 2025:2040))

# INSPECT short-term performance

perf[year == 2025, .(value=mean(data)), by=.(mp, statistic)]

# PLOT

ggplot(perf[year == 2025,], aes(x=mp, y=data)) + geom_boxplot(aes(fill=mp)) +
  facet_wrap(~name, scales="free")

ggplot(perf[year == 2040,], aes(x=mp, y=data)) + geom_boxplot(aes(fill=mp)) +
  facet_wrap(~name, scales="free")

# TRACKING

tracking(trend)['time', ]
tracking(ices)['time', ]

