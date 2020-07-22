#function to simulate data and run JAGS model
runJAGSmodel<-function(dataIn, modIn,declineRate,nyrs, nvisits,nsim,scenario,speciesName){
  
  new.data<-dataIn
  
  #get refuge code
  refugeName<-as.character(unique(new.data$NWR_code))
  
  mod.1<-modIn
  
  #get coefficients from mod.1 (run previously in 'unmarked')
  coefs<-coef(mod.1)
  
  #########################################################################################
  #get multinomial detection probs
  
  #for model with effects of Visit on detection probability
  det.probs<-getP(mod.1)
  
  ##########################################################################################################################################
  #set declineRate  
  surv<-switch(declineRate,
               "-0.10"= 0.6045,
               "-0.25"= 0.5785,
               "-0.50"= 0.5065)
  
  rec<-switch(declineRate,
              "-0.10" = 0.3,
              "-0.25" = 0.2,
              "-0.50" = 0.1)
  
  Beta<- log(surv+rec)
  
  ##########################################################################################################################################
  #get nVisits
  nVisits=nvisits
  
  #get list of UnitNum
  UnitNumList<-sort(unique(new.data$UnitNum))
  
  #get number of points per unit
  n.points.out<-list()
  for (i in 1:length(UnitNumList)){
    new.data.2<-new.data
    sub.data<-subset(new.data.2, UnitNum==UnitNumList[i])
    unit.points<-length(unique(sub.data$PointID))
    output<-data.frame(UnitNum=UnitNumList[i], unit.points=unit.points)
    n.points.out<-rbind(n.points.out, output)
  }
  
  #get proportion of points per unit
  n.points.out$Proportion<-n.points.out$unit.points/sum(n.points.out$unit.points)
  #get n.points (unit.points x nVisits)
  n.points.out$n.points<-n.points.out$unit.points*nVisits
  
  #total points within refuge
  npoints<-sum(n.points.out$n.points)
  
  ## get coefficient estimates to be used in data simulation
  beta<-coef(mod.1)
  betaBIRD<-beta[c("lambda(Int)")]
  
  ##predict expected abundance per point count on log-scale
  Xspr<-cbind(rep(1,npoints))
  lam<-Xspr%*%(betaBIRD)
  #predict(mod.1, type="lambda")
  
  #get parameters for detection function
  dparm<-beta[c("p(Int)")]
  sigma<-exp(Xspr%*%dparm)
  
  #number of sampling points
  J<-npoints 
  
  ##########################################################################################################################################
  #set parameters for the different scenarios
  
  lamnew<-exp(lam)
  
  #number of sampling points
  npoints<- J #modify for testing scenarios with varying number of survey locations
  
  ############################################
  #Begin simulation loop
  for (i in 1:nsim) {
    print(i)
    
    #########################################################################################
    #simulate data
    
    dat<-switch(scenario,
     "FullPanel"= dataSimFullPanel(dataIn=new.data,modIn=modIn,declineRate=declineRate,nyrs=nyrs,nvisits=nvisits),
     "HalfPanel"= dataSimFullPanel(dataIn=new.data,modIn=modIn,declineRate=declineRate,nyrs=nyrs,nvisits=nvisits),
     "NSrotatingFull"= dataSimFullPanel(dataIn=new.data,modIn=modIn,declineRate=declineRate,nyrs=nyrs,nvisits=nvisits),
     "NSrotationHalf"= dataSimFullPanel(dataIn=new.data,modIn=modIn,declineRate=declineRate,nyrs=nyrs,nvisits=nvisits),
     "OneRegionRotating"= dataSimFullPanel(dataIn=new.data,modIn=modIn,declineRate=declineRate,nyrs=nyrs,nvisits=nvisits),
     "NStwoRegionRotating"= dataSimFullPanel(dataIn=new.data,modIn=modIn,declineRate=declineRate,nyrs=nyrs,nvisits=nvisits),
     message("Error! Please choose from the following design scenarios: (FullPanel,HalfPanel, NSrotatingFull, NSrotatingHalf,
             OneRegionRotating,NStwoRegionRotating)")
    )
    
    #########################################################################################
    #get rowSums of detection data y
    y<-sapply(dat$detList, rowSums)
    
    #get covariates (SMI_Unit and VisitNum)
    covs<-as.matrix(dat$covList)
    
    #make covariates continuous integers (SMI_Unit)
    a<-as.character(levels(factor(covs[,1])))
    newUnit<-covs[,1]
    for(j in 1:length(a)){
      newUnit[which(covs[,1]==a[j])]<-j
    }
    newUnit<-as.integer(newUnit)
    
    #make covariates continuous (visit)
    a<-as.integer(levels(factor(covs[,2])))
    newVisit<-covs[,2]
    for(j in 1:length(a)){
      newVisit[which(covs[,2]==a[j])]<-j
    }
    newVisit<-as.integer(newVisit)
    
    #get number of unique units
    nUnits<-length(unique(newUnit))
    
    nVisits=nvisits
    
    ############################################
    #Prepare data for JAGS
    #bundle data
    data <- list(y = y, M = nrow(y), nUnits=nUnits, nYears=nyrs,  nVisits=nVisits, unit=newUnit, visit=newVisit)
    #inspect structure of data
    str(data)                  
    
    yin<-y+1
    
    # Initial values
    inits <- function(){
      list (p0 = runif(1), beta0 = runif(1), N=yin)
    }
    
    # Parameters monitored
    parameters <- c("p0", "alpha0", "alpha1","beta0","beta2","gamma")
    
    # MCMC settings
    nc <- 3   ;   ni <- 20000   ;   nb <- 5000   ;   nt <- 20
    
    ############################################
    #run with 'rjags'
    mod<-jags.model("JAGS.model.txt", data, inits, n.chains = nc, n.adapt=500)
    out<-coda.samples(mod, parameters,thin=nt, n.iter=ni)
    res<-summary(window(out, start=1001))
    out$value$model
    
    if(is.numeric(try(max(gelman.diag(out)[[1]][,1]), silent=T))) Rmax<-max(gelman.diag(out)[[1]][,1]) else Rmax<-NA
    
    dir.create(paste(getwd(),"Results","BAYES_PWR",scenario,speciesName,refugeName, paste("Power","_", speciesName,"_",refugeName,"_",round(Beta, dig=2), "decline_",nyrs,"_years" ,sep=''),sep="/"))
    saveFilepath<-paste(getwd(),"Results","BAYES_PWR",scenario,speciesName,refugeName,paste("Power","_",speciesName,"_",refugeName ,"_",round(Beta, dig=2), "decline_",nyrs,"_years", sep=''),sep="/")
    
    dput( list(data=dat, model=res,R=Rmax, mcs=out),
          file=paste(saveFilepath, paste("JAGS_",scenario,speciesName,"_",refugeName,"_", npoints, "_", round(Beta, dig=2), "decline_", nyrs,"_years",i,".R", sep=''),sep="/")   )
    
  }  #end of First sim loop
  
  #now harvest results
  message("Now compiling and saving simulation results.")
  
  #set model
  mod.1<-modIn
  
  # get coefficient estimates used in data simulation
  beta<-coef(mod.1)
  
  #filepath for JAGS model no CAR
  saveFilepath<-paste(getwd(),"Results","BAYES_PWR",scenario, speciesName,refugeName,paste("Power","_",speciesName,"_",refugeName ,"_",round(Beta, dig=2), "decline_",nyrs,"_years", sep=''),sep="/")
  
  str<-c(paste(saveFilepath, paste("JAGS_",scenario,speciesName,"_",refugeName,"_", npoints, "_", round(Beta, dig=2), "decline_", nyrs,"_years",1:nsim,".R", sep=''),sep="/"))
  truebeta<-c(beta[1],beta[5:(nUnits+3)],rep(Beta,nUnits),beta[nUnits+4], beta[nUnits+7])
  
  BetaRes<-array(0, c(nsim, 6, (nUnits*2+3)))
  
  new.betaList<-list()
  betaList<-for(i in 1:nUnits){
    new.betaList[[i]]<-c(paste("beta2","[",i,"]",sep=""))
  }
  
  new.gammaList<-list()
  gammaList<-for(i in 1:nUnits){
    new.gammaList[[i]]<-c(paste("gamma","[",i,"]",sep=""))
  }
  
  dimnames(BetaRes)<-list((1:nsim), c('Mean','p', 'Bias','Lower','Upper', 'Coverage'),
                          c("lambda(Int)",
                            new.betaList,
                            new.gammaList,
                            "p(Int)", "alpha(alpha)") )
  
  for(i in 1:nsim){
    
    print(i)
    
    out<-source(str[i])
    mod<-out$value$model
    
    coefs<-mod$statistics[,1]
    ord<-c(seq(5,(nUnits*2+6)),1)
    CI<-mod$quantiles[ord,c(1,5)]
    
    #coef values
    BetaRes[i,1,]<-coefs[ord]
    
    #bias
    BetaRes[i,3,]<-(coefs[ord]-truebeta)/truebeta
    
    #lower CI
    BetaRes[i,4,]<- CI[,1]
    
    #upper CI
    BetaRes[i,5,]<- CI[,2]
    
    BetaRes.df<-as.data.frame(BetaRes)
    #coverage, p-value
    for ( k in 1:length(truebeta)) {
      if (truebeta[k] >= BetaRes[i,4,k] & truebeta[k] <= BetaRes[i,5,k] ) BetaRes[i,6,k]<-1 else BetaRes[i,6,k]<-0
      if(CI[k,1]<0 & CI[k,2]>0)    BetaRes[i,2,k]<-0 else  BetaRes[i,2,k]<-1
    }
    
    #p-value for gamma
    for(h in (nUnits+1):(nUnits*2+1)){
      if(CI[h,2]<1)    BetaRes[i,2,h]<-1 else  BetaRes[i,2,h]<-0
    }
  }
  
  Restab=matrix(0, nrow=(nUnits*2+3), ncol=5)
  colnames(Restab)=c('Mean','rmse','BiasMean','CI', 'significance')
  rownames(Restab)=c("lambda(Int)",
                     new.betaList,
                     new.gammaList,
                     "p(Int)", "alpha(alpha)")
  
  #means
  Restab[,1]<-apply(BetaRes[,1,], 2, mean)
  
  #rmse
  for (k in 1:(nUnits*2+3)){
    intm<-sqrt(sum((BetaRes[,1,k]-truebeta[k])^2)/nsim)
    Restab[k,2]<-intm
  }
  
  #bias
  Restab[,3]<-apply(BetaRes[,3,],2,mean)
  
  #Coverage
  Restab[,4]<-apply(BetaRes[,6,],2,sum)
  
  #significance
  Restab[,5]<-apply(BetaRes[,2,],2,sum)
  
  Restab<-as.data.frame(Restab)
  Restab$pwr<-Restab$significance/nsim
  
  #get beta.names
  beta.names<-names(beta)[c(1,5:(nUnits+3))]
  beta.names.1<-data.frame(beta.names=beta.names)
  beta.names.1$beta.names<-as.character(beta.names.1$beta.names)
  beta.names.2<-gsub("lambda[(]", "", beta.names.1$beta.names)
  beta.names.2<-gsub(")", "", beta.names.2)
  beta.names.3<-gsub("UnitNum", "", beta.names.2)
  beta.names.out<-data.frame(UnitNum=beta.names.3)
  
  #get unit table
  unit.table<-unique(new.data[,c("UnitNum","SMI_Unit")])
  
  unit.table.1<-merge(beta.names.out,unit.table, by=c("UnitNum"),all=TRUE)
  unit.table.1$UnitNum<-as.character(unit.table.1$UnitNum)
  
  #replace "Int" with remaining UnitNum
  missing.unit<-as.character(setdiff(unit.table$UnitNum,unit.table.1$UnitNum))
  
  unit.table.1$UnitNum[is.na(unit.table.1$UnitNum)] <-missing.unit
  unit.table.out.1<-na.omit(unit.table.1)
  unit.table.out<-rbind(unit.table.out.1[nUnits,],unit.table.out.1[1:(nUnits-1),])
  unit.table.out<-data.frame(SMI_Unit=unit.table.out$SMI_Unit, UnitNum=unit.table.out$UnitNum)
  
  #format results as table
  abun.est<-Restab[2:(nUnits+1),]
  abun.est$Predicted<-exp(abun.est$Mean)
  abun.est.1<-cbind(unit.table.out, abun.est)
  abun.est.out<-abun.est.1[,c("SMI_Unit","UnitNum","Mean","rmse","BiasMean","Predicted")]
  
  #save abundance estimates
  write.csv(abun.est.out, file=paste(saveFilepath, paste(speciesName, refugeName, "JAGS.abundance.output.csv",sep="_"),sep="/"))
  
  power.table<-Restab[(nUnits+2):(nUnits*2+1),]
  power.table$model_Type<-paste("Bayes_power","_",speciesName,"_", refugeName,"_",npoints,"_", round(Beta, dig=2), "decline_", nyrs,"_years", sep='')
  power.table$NumPoints<-npoints/nvisits
  power.table$nVisits<-nvisits
  power.table.1<-data.frame(power.table,abun.est.out[,c("SMI_Unit","UnitNum","Predicted")])
  power.table.out<-power.table.1[,c("SMI_Unit","UnitNum","NumPoints","nVisits","Predicted","Mean","rmse","BiasMean","CI","significance","pwr","model_Type")]
  colnames(power.table.out)[5]<-"Predicted.abun"
  colnames(power.table.out)[6]<-"Mean_gamma"
  #save results
  
  write.csv(Restab, file=paste(saveFilepath, "summary.output.csv",sep="/"))
  
  write.csv(power.table.out, file=paste(saveFilepath, "power.table.out.csv",sep="/"))
  
}

