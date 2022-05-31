library(ss3om)
library(FLa4a)

setwd("~/github/mse_fleets_WK")
nit <- 20
ang <- readOutputss3('data/ang/')

stk <- buildFLSss330(ang)

stk <- simplify(stk, "unit")
plot(stk)


idx <- buildFLIBss330(ang)

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


# EXAMPLE FLIndexBiomass with different time horizons


idxE = FLQuants(Map(function(x,y){
  rlnorm(nit,index.q(x)%=%0,y)
},x=fwdWindow(idx,end=2040),y=c(0.2,0.25,0.22)))





idx[2:3] = fwdWindow(idx[2:3], end=2040)


oemB <- propagate(FLoem(method=sampling.oem,
                       observations=list(stk=stk, idx=idx),
                       deviances=list(idx=idxE)),nit)


plot(oemB@observations$idx)
plot(index.q(oemB@observations$idx))
 






# 
fmod  <- ~factor(year) + factor(age)

srmod <- ~factor(year)

n1mod  <- ~factor(age)

qmod <- list(
	   ~s(age, k = 6), # BTS-Isis-early
	   ~s(age, k = 6), # BTS-Combined (ISIS and TRIDENS)
	   ~s(age, k = 5), # SNS
	   ~s(age, k = 6), # BTS-Combined (all)
	   ~s(age, k = 6), # IBTS_Q3
	   ~s(age, k = 5)) # IBTS_Q1

vmod <- list(
	~s(age, k = 3),
	   ~1, # BTS-Isis-early
	   ~1, # BTS-Combined (ISIS and TRIDENS)
	   ~1, # SNS
	   ~1, # BTS-Combined (all)
	   ~1, # IBTS_Q3
	   ~1) # IBTS_Q1

fit2 <- sca(stock = ple4, indices = ple4.indices, fmodel=fmod, srmodel=srmod, n1model=n1mod, qmodel=qmod, vmodel=vmod)
