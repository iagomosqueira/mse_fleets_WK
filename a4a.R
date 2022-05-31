library(mse)
library(ss3om)
library(ggplotFL)
library(FLa4a)
# SET final year & iters



#fy <- 2040
nit <- 1



# LOAD anglerfish SS3 run



ang <- readOutputss3('data/ang/')
stk_org <- buildFLSss330(ang)
stk_sexcomb <- simplify(stk_org, 'unit')



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



fmodel <- ~s(year,k=35)+s(age,k=18)
qmodel <- list(~s(age,k=18),~s(age,k=18))
srmodel <- ~s(year,k=35)



fit1 <- sca(stk_sexcomb,surE,fmodel=fmodel,qmodel=qmodel,srmodel=srmodel)
a4a <- stk_sexcomb+fit1



plot(FLStocks("ss3"=stk_sexcomb,"a4a"=a4a))



res <- residuals(fit1,stk_sexcomb,surE)
plot(computeCatchDiagnostics(fit1,stk_sexcomb))
plot(res)
plot(fit1,surE[2])
save(fit1,file="a4a_ang.RData")

