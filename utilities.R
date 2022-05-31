# utilities.R - DESC
# /home/mosqu003/Active/WORKSHOP_JRC_MSE-MAY2022/mse_fleets_WK/utilities.R

# Copyright (c) WUR, 2022.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# metrics

mets <- list(
  Rec=function(x) unitSums(rec(x)),
  SSB=function(x) unitSums(ssb(x)),
  F=function(x) unitMeans(fbar(x)),
  Catch=function(x) unitSums(catch(x))
)

