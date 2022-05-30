
library(mse)
library(ss3om)
library(ggplotFL)
library(FLa4a)
# SET final year & iters

fy <- 2040
nit <- 1

# LOAD anglerfish SS3 run

ang <- readOutputss3('data/ang/')
stk <- buildFLSss330(ang)
stk <- simplify(stk, 'unit')

stock.wt(stk)
plot(stk)
## iNDEX

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

index.q(sur[[1]]) <- iqs[[1]]
index.q(sur[[2]]) <- iqs[[2]]

sur[[1]] <- survey(stk, sur[[1]])
sur[[2]] <- survey(stk, sur[[2]])

surE = FLIndices(Map(function(x,y){
  x@index = x@index*rlnorm(nit,x@index%=%0,y)
  x
},x=sur,y=c(0.23,0.27)) ) 



fit1 <- sca(stk,surE[1])
