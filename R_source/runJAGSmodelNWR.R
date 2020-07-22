#function to simulate data and run JAGS model
runJAGSmodelNWR<-function(dataIn, modIn,declineRate,nyrs, nvisits,nsim,speciesName){

  new.data<-dataIn

  #get refuge code
  regionName<-as.character(unique(new.data$RegionName))

  #define model from unmarked
  mod.1<-modIn

  nVisits=nvisits
  # nsim=3
  # nyrs=5
  # speciesName="SALS"
 
   #add column for refugeNum
  new.data$NWR_code<-as.factor(as.character(new.data$NWR_code))
  
  new.data$refugeNum<-as.integer(new.data$NWR_code)
  
  refugeNumList<-sort(unique(new.data$refugeNum))
  
  #get number of points per unit
  n.points.out<-list()
  for (i in 1:length(refugeNumList)){
    new.data.2<-new.data
    sub.data<-subset(new.data.2, refugeNum==refugeNumList[i])
    refuge.points<-length(unique(sub.data$PointID))
    output<-data.frame(refugeNum=refugeNumList[i], refuge.points=refuge.points)
    n.points.out<-rbind(n.points.out, output)
  }
  
  #get proportion of points per refuge
  n.points.out$Proportion<-n.points.out$refuge.points/sum(n.points.out$refuge.points)
  #get n.points (refuge.points x nVisits)
  n.points.out$n.points<-n.points.out$refuge.points*nVisits
  
  #total points within refuge
  npoints<-sum(n.points.out$n.points)
  
  ############################################
  #Begin simulation loop
  for (i in 1:nsim) {
    print(i)

    #########################################################################################
    #simulate data

    dat<-dataSimRegionFull(dataIn=new.data,modIn=mod.1,declineRate=declineRate,nyrs=nyrs,nvisits=nVisits)

    #########################################################################################
    #get rowSums of detection data y
    y<-sapply(dat$detList, rowSums)

    #get covariates (NWR_code and VisitNum)
    covs<-as.matrix(dat$covList)

    #make covariates continuous integers (NWR_code)
    a<-as.character(levels(factor(covs[,1])))
    newRefuge<-covs[,1]
    for(j in 1:length(a)){
      newRefuge[which(covs[,1]==a[j])]<-j
    }
    newRefuge<-as.integer(newRefuge)

    #make covariates continuous (visit)
    a<-as.integer(levels(factor(covs[,2])))
    newVisit<-covs[,2]
    for(j in 1:length(a)){
      newVisit[which(covs[,2]==a[j])]<-j
    }
    newVisit<-as.integer(newVisit)

    #get number of unique units
    nRefuges<-length(unique(newRefuge))

    ############################################
    #Prepare data for JAGS
    #bundle data
    data <- list(y = y, M = nrow(y), nRefuges=nRefuges, nYears=nyrs,  nVisits=nVisits, refuge=newRefuge, visit=newVisit)
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
    nc <- 3   ;   ni <- 10000  ;   nb <- 1000  ;   nt <- 20

    ############################################
    #run with 'rjags'
    mod<-jags.model("JAGS.model.NWR.txt", data, inits, n.chains = nc, n.adapt=500)
    out<-coda.samples(mod, parameters,thin=nt, n.iter=ni)
    res<-summary(window(out, start=1001))
    out$value$model

    if(is.numeric(try(max(gelman.diag(out)[[1]][,1]), silent=T))) Rmax<-max(gelman.diag(out)[[1]][,1]) else Rmax<-NA

    dir.create(paste(getwd(),"Results","SMI_Full","Power_Bayes",speciesName,regionName,sep="/"))
    dir.create(paste(getwd(),"Results","SMI_Full","Power_Bayes",speciesName,regionName,paste("SMI_Full_","Power","_", speciesName,"_",regionName,"_",declineRate, "decline_",nyrs,"_years" ,sep=''),sep="/"))
    saveFilepath<-paste(getwd(),"Results","SMI_Full","Power_Bayes",speciesName,regionName,paste("SMI_Full_", "Power","_",speciesName,"_",regionName ,"_",declineRate, "decline_",nyrs,"_years", sep=''),sep="/")

    dput( list(data=dat, model=res,R=Rmax, mcs=out),
          file=paste(saveFilepath, paste("JAGS_",speciesName,"_",regionName,"_", npoints, "_", declineRate, "decline_", nyrs,"_years",i,".R", sep=''),sep="/")   )

  }  #end of First sim loop

  #########################################################################################

  #now harvest results
  message("Now compiling and saving simulation results.")

  # #add column for refugeNum
  # new.data$NWR_code<-as.factor(as.character(new.data$NWR_code))
  # 
  # new.data$refugeNum<-as.integer(new.data$NWR_code)
  # 
  # refugeNumList<-sort(unique(new.data$refugeNum))

  #get coefficients from mod.1 (run previously in 'unmarked')
  # get coefficient estimates to be used in data simulation
  beta<-coef(mod.1)
  beta.names<-names(beta)

  beta.use<-beta[1:length(refugeNumList)]
  try(beta0<-coef(mod.1)[1])
  try(beta1<-coef(mod.1)[1]+coef(mod.1)[2])
  try(beta2<-coef(mod.1)[1]+coef(mod.1)[3])
  try(beta3<-coef(mod.1)[1]+coef(mod.1)[4])
  try(beta4<-coef(mod.1)[1]+coef(mod.1)[5])

  coefs<-if(length(refugeNumList)==1){
    c(beta0, beta[(length(refugeNumList)+1):(length(refugeNumList)+4)])
  }else{if(length(refugeNumList)==2){
    c(beta0, beta1, beta[(length(refugeNumList)+1):(length(refugeNumList)+4)])
  }else{if(length(refugeNumList)==3){
    c(beta0, beta1, beta2, beta[(length(refugeNumList)+1):(length(refugeNumList)+4)])
  }else{if(length(refugeNumList)==4){
    c(beta0, beta1, beta2,beta3, beta[(length(refugeNumList)+1):(length(refugeNumList)+4)])
  }else{if(length(refugeNumList)==5){
    c(beta0, beta1, beta2,beta3,beta4, beta[(length(refugeNumList)+1):(length(refugeNumList)+4)])
  }}}}}
  names(coefs)<-beta.names
  #########################################################################################
  #get multinomial detection probs

  #for model with effects of Visit on detection probability
  det.probs<-getP(mod.1)

  ##########################################################################################################################################
  #set declineRate
  surv<-switch(declineRate,
               "-0.10"= 0.6045,
               "-0.15"= 0.6045,
               "-0.20"= 0.6045,
               "-0.25"= 0.5788,
               "-0.30"= 0.498,
               "-0.35"= 0.45,
               "-0.40"= 0.42,
               "-0.45"= 0.398,
               "-0.50"= 0.5065)

  rec<-switch(declineRate,
              "-0.10" = 0.30,
              "-0.15" = 0.2562,
              "-0.20" = 0.214,
              "-0.25"= 0.20,
              "-0.30"= 0.2428,
              "-0.35"= 0.2547,
              "-0.40"= 0.2503,
              "-0.45"= 0.2396,
              "-0.50"= 0.1)

  Beta<- round(log(surv+rec),2)

  ##########################################################################################################################################
  #get nVisits
  nVisits=nvisits

  
  #get number of points per unit
  n.points.out<-list()
  for (i in 1:length(refugeNumList)){
    new.data.2<-new.data
    sub.data<-subset(new.data.2, refugeNum==refugeNumList[i])
    refuge.points<-length(unique(sub.data$PointID))
    output<-data.frame(refugeNum=refugeNumList[i], refuge.points=refuge.points)
    n.points.out<-rbind(n.points.out, output)
  }

  #get proportion of points per refuge
  n.points.out$Proportion<-n.points.out$refuge.points/sum(n.points.out$refuge.points)
  #get n.points (refuge.points x nVisits)
  n.points.out$n.points<-n.points.out$refuge.points*nVisits

  #total points within refuge
  npoints<-sum(n.points.out$n.points)

  # ## get coefficient estimates to be used in data simulation
  #
  #
  #
  #
  # beta<-coef(mod.1) #may have to modify these!
  # betaBIRD<-beta[c("lambda(Int)")]
  #
  # ##predict expected abundance per point count on log-scale
  # Xspr<-cbind(rep(1,npoints))
  # lam<-Xspr%*%(betaBIRD)
  # #predict(mod.1, type="lambda")
  #
  # #get parameters for detection function
  # dparm<-beta[c("p(Int)")]
  # sigma<-exp(Xspr%*%dparm)
  #
  # #number of sampling points
  # J<-npoints
  #
  # ##########################################################################################################################################
  # #set parameters for the different scenarios
  #
  # lamnew<-exp(lam)

  #number of sampling points
  #npoints<- J #modify for testing scenarios with varying number of survey locations


  # get coefficient estimates used in data simulation
  beta<-coefs  #add column for refugeNum
  new.data$NWR_code<-as.factor(as.character(new.data$NWR_code))
  
  new.data$refugeNum<-as.integer(new.data$NWR_code)
  
  refugeNumList<-sort(unique(new.data$refugeNum))
  

  #filepath for JAGS model no CAR
  saveFilepath<-paste(getwd(),"Results","SMI_Full","Power_Bayes",speciesName,regionName,paste("SMI_Full_", "Power","_",speciesName,"_",regionName ,"_",declineRate, "decline_",nyrs,"_years", sep=''),sep="/")

  str<-c(paste(saveFilepath, paste("JAGS_", speciesName,"_",regionName,"_", npoints, "_", declineRate, "decline_", nyrs,"_years",1:nsim,".R", sep=''),sep="/"))
  truebeta<-c(beta[1:nRefuges],rep(Beta,nRefuges),beta[nRefuges+1], beta[nRefuges+4])

  BetaRes<-array(0, c(nsim, 6, (nRefuges*2+2)))

  new.betaList<-list()
  betaList<-for(i in 1:nRefuges){
    new.betaList[[i]]<-c(paste("beta2","[",i,"]",sep=""))
  }

  new.gammaList<-list()
  gammaList<-for(i in 1:nRefuges){
    new.gammaList[[i]]<-c(paste("gamma","[",i,"]",sep=""))
  }

  dimnames(BetaRes)<-list((1:nsim), c('Mean','p', 'Bias','Lower','Upper', 'Coverage'),
                          c(new.betaList,
                            new.gammaList,
                            "p(Int)", "alpha(alpha)") )

  for(i in 1:nsim){

    print(i)

    out<-source(str[i])
    mod<-out$value$model

    coefs<-mod$statistics[,1]
    ord<-c(seq(6,(nRefuges*2+6)),1)
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
    for(h in (nRefuges+1):(nRefuges*2+1)){
      if(CI[h,2]<1)    BetaRes[i,2,h]<-1 else  BetaRes[i,2,h]<-0
    }
  }

  Restab=matrix(0, nrow=(nRefuges*2+2), ncol=5)
  colnames(Restab)=c('Mean','rmse','BiasMean','CI', 'significance')
  rownames(Restab)=c(
                     new.betaList,
                     new.gammaList,
                     "p(Int)", "alpha(alpha)")

  #means
  Restab[,1]<-apply(BetaRes[,1,], 2, mean)

  #rmse
  for (k in 1:(nRefuges*2+2)){
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
  beta.names<-names(beta)[1:nRefuges]
  beta.names.1<-data.frame(beta.names=beta.names)
  beta.names.1$beta.names<-as.character(beta.names.1$beta.names)
  beta.names.2<-gsub("lambda[(]", "", beta.names.1$beta.names)
  beta.names.2<-gsub(")", "", beta.names.2)
  beta.names.3<-gsub("refugeNum", "", beta.names.2)
  beta.names.out<-data.frame(refugeNum=beta.names.3)
  beta.names.out$refugeNum<-as.integer(beta.names.out$refugeNum)
  
  #get unit table
  ref.table<-unique(new.data[,c("refugeNum","NWR_code")])

  ref.table.1<-merge(beta.names.out,ref.table, by=c("refugeNum"),all=TRUE)
  ref.table.1$refugeNum<-as.character(ref.table.1$refugeNum)
  ref.table.out<-data.frame(NWR_code=ref.table.1$NWR_code, refugeNum=ref.table.1$refugeNum)

  #format results as table
  abun.est<-Restab[1:nRefuges,]
  abun.est$Predicted<-exp(abun.est$Mean)
  abun.est.1<-cbind(ref.table.out, abun.est)
  abun.est.out<-abun.est.1[,c("NWR_code","refugeNum","Mean","rmse","BiasMean","Predicted")]
  #save abundance estimates
  write.csv(abun.est.out, file=paste(saveFilepath, "abun.out.csv",sep="/"),row.names=TRUE)
  
  power.table<-Restab[(nRefuges+1):(nRefuges*2),]
  power.table$model_Type<-paste("SMI_Full","_Power_Bayes","_",speciesName,"_", regionName,"_",npoints,"_", round(Beta, dig=2), "decline_", nyrs,"_years", sep='')
  power.table$NumPoints<-npoints/nvisits
  power.table$nVisits<-nvisits
  power.table.1<-data.frame(power.table,abun.est.out[,c("NWR_code","refugeNum","Predicted")])
  power.table.out<-power.table.1[,c("NWR_code","refugeNum","NumPoints","nVisits","Predicted","Mean","rmse","BiasMean","CI","significance","pwr","model_Type")]
  colnames(power.table.out)[5]<-"Predicted.abun"
  colnames(power.table.out)[6]<-"Mean_gamma"
  #save results

  write.csv(Restab, file=paste(saveFilepath, "summary.output.csv",sep="/"),row.names=TRUE)

  write.csv(power.table.out, file=paste(saveFilepath, "power.table.out.csv",sep="/"),row.names=TRUE)

}

