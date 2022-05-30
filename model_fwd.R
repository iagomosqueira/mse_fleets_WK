# model_fwd.R - DESC
# /home/mosqu003/Active/WORKSHOP_JRC_MSE-MAY2022/model_fwd.R

# Copyright (c) WUR, 2022.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)

load('data/om_stock.Rdata')

# INSPECT om

# FWD

ctrl <- fwdControl(
  list()
)

run <- fwd(om, 


