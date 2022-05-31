# SPM fits to assessment data

load(file="data/om_stock.Rdata",verbose=T)

library(JABBA)

# Z estimator Chapman and Robsen
zcr =  function(flq,frange = "missing"){
  out=NULL
  age = an(dimnames(flq)[[1]])
  if(missing(frange)){
    Amin = age[which(apply(flq,1,median)==max(apply(flq,1,median),na.rm=T))]+1
    Amax = age[nrow(flq)]
    frange= Amin:Amax
  }
  for(t in 1:ncol(flq)){
    Ct= flq[,t]
    Ct[Ct==0] = NA
    df =   as.data.frame((Ct[ac(frange),])) 
    age.r <- df$age-min(df$age,na.rm=TRUE)
    nc <- sum(df$data,na.rm=TRUE)
    Tc <- sum(age.r *df$data,na.rm=TRUE)
    S.est <- Tc/(nc+Tc-1)
    Z =  -log(S.est)
    ## Estimate Z and SE
    df = df[1,]
    df$data = Z
    df$age = "all"
    out = rbind(out,df)
  }
    return(as.FLQuant(out,quant="age"))
}

# Make JABBA input 
stk = iter(window(simplify(stock(om), 'unit'),end=2020),1)
SurveyAge = window(iter(surSim[[2]]@index,1),end=2020)
# Biomass indices
Idxs = iter(window(oemB@observations$idx,end=2020),1)


# Catch age (with error) - like a4a input
Zc = zcr(catch.n(stk)*rlnorm(1,catch.n(stk)%=%0,0.1),frange = 3:15)
# Survey with error
Zs= window(zcr(SurveyAge,frange=3:15))

plot(Zc,Zs,quantMeans(z(stk)[ac(3:15)]))


df = rbind(
  data.frame(as.data.frame(catch(stk)),qname="Catch"),
  as.data.frame(index(Idxs)),
  as.data.frame(FLQuants(Zc=Zc,Zs=Zs))

)



inp = reshape2::dcast(df,year~qname,value.var="data",fun.aggregate=sum)
Index = inp[,c(1,3:5)]
Index[Index==0] = NA
Catch = inp[,c("year","Catch")]
Z= inp[,c("year","Zs","Zc")]
Z[Z==0] = NA

# Build JABBA
# use Fmsy as prior mean for r = fmsy in Fox model
fmsy = an(om@refpts["FMSY"])
bmsy = an(om@refpts["SBMSY"])
b0 = an(om@refpts["SB0"])
# 1. Catch + Indices
jbI = build_jabba(catch=Catch, cpue=Index,
             assessment = "Monk", scenario = "Catch+Index",
              model.type = "Fox",
              r.prior = c(0.2,0.3),verbose = F,
              igamma=c(0.001,0.001),fixed.obsE = 0.15)
# Fit JABBA (fast mcmc)
fI = fit_jabba(jbI,quickmcmc = T,do.ppc = T)

# 2. Catch + Indices+ Zs
jbZ = build_jabba(catch=Catch, cpue=Index,auxiliary = Z[,1:2],
                  assessment = "Monk", scenario = "Catch+Index+Zs",
                  model.type = "Fox",
                  auxiliary.type = "z",
                  auxiliary.lag = 0,
                  auxiliary.sigma = F,
                  auxiliary.obsE = 0.1, # need forcing of trend
                  sigma.est = T,
                  fixed.obsE = 0.15, 
                  r.prior = c(0.2,0.3),verbose = F,
                  igamma=c(0.001,0.001))
# Fit JABBA (fast mcmc)
fZ = fit_jabba(jbZ,quickmcmc = T,do.ppc = T)
jbplot_ensemble(list(fI,fZ))
jbplot_ppdist(fZ)
jbplot_cpuefits(fZ)
jbplot_runstest(fZ)

jbplot_summary(list(fI,fZ))

# Compare: Black dashed line is TRUE pop dyn from OM
jbpar(mfrow=c(2,2))
jbplot_ensemble(list(fI,fZ),subplots = 1,add=T)
#lines(data~year,data=as.data.frame(sqrt(ssb(stk)/bmsy)),lwd=2,lty=2)
abline(v=2000,lty=2)
#lines(data~year,data=as.data.frame(ssb(stk)/(0.125*b0)),lwd=1,lty=1)
jbplot_ensemble(list(fI,fZ),subplots = 2,add=T)
abline(v=2000,lty=2)
lines(data~year,data=as.data.frame(fbar(stk)/fmsy),lwd=2,lty=2)
jbplot_ensemble(list(fI,fZ),subplots = 5,add=T)
abline(v=2000,lty=2)
jbplot_ensemble(list(fI,fZ),subplots = 6,add=T)
abline(v=2000,lty=2)


# MP 
est = mp_jabba(jbZ,par.quantile = 0.3)

ggplot(est$B,aes(year,data))+geom_line()+ylab("Biomass")
TAC = est$refpts[["Fmsy"]]*tail(est$B$data,1)
     