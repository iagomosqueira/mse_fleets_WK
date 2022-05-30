# model_sca.R - DESC
# /model_sca.R

# Copyright Iago MOSQUEIRA (WMR), 2022
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# XX {{{
# }}}


# --- RUN FLa4a ICES HCR and TAC short-term forecast

library(FLa4a)

# a4a MODEL

# fmodel
fmod <- ~te(replace(age, age > 8, 8), year, k = c(4, 22)) +
  s(replace(age, age > 8, 8), k=4) + s(year, k=22, by=as.numeric(age==1))

# qmodel (BTS, SNS)
qmod <- list(~s(age, k=3), ~s(age, k=3))

# vmodel (catch, BTS, SNS)
vmod <- list(~s(age, k=3), ~s(age, k=3), ~s(age, k=3))

# srmodel
srmod <- ~factor(year)

# control: sca.sa + ices.hcr + tac.is

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=sca.sa, args=list(fmodel=fmod, qmodel=qmod,
    vmodel=vmod, srmodel=srmod)),
  # CCSBT trend HCR
  hcr = mseCtrl(method=ices.hcr,
    args=list(ftrg=c(refpts(om)$Fmsy), sblim=c(refpts(om)$Blim),
      sbsafe=c(refpts(om)$Bpa))),
  isys=mseCtrl(method=tac.is, args=list(dtaclow=0.85, dtacupp=1.15, recyrs=30))
  ))

# RUN mp

scamp <- mp(om, oem=oem, ctrl=control, args=mseargs)


