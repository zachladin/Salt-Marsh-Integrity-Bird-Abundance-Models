#function to simulate data and run JAGS model
runJAGSmodelUnits_Visit<-function(dataIn, modIn,declineRate,nyrs, nvisits,nsim,speciesName){

  new.data<-dataIn

  #get refuge code
  refugeName<-as.character(unique(new.data$NWR_code))

  #define model from unmarked
  mod.1<-modIn

  nvisits=3
  nsim=3
  nyrs=5
  speciesName="SALS"
  declineRate="-0.50"

  nVisits=nvisits

   #add column for unitNum
  #new.data$SMI_Unit<-as.factor(as.character(new.data$SMI_Unit))

  #new.data$unitNum<-as.integer(new.data$SMI_Unit)

  unitNumList<-sort(unique(new.data$unitNum))

  #get number of points per unit
  n.points.out<-list()
  for (i in 1:length(unitNumList)){
    new.data.2<-new.data
    sub.data<-subset(new.data.2, unitNum==unitNumList[i])
    unitName<-unique(as.character(sub.data$SMI_Unit))
    unit.points<-length(unique(sub.data$PointID))
    output<-data.frame(unitName=unitName,unitNum=unitNumList[i], unit.points=unit.points)
    n.points.out<-rbind(n.points.out, output)
  }

  n.points.for.units<-n.points.out[,c("unitName","unit.points")]

  #get proportion of points per refuge
  n.points.out$Proportion<-n.points.out$unit.points/sum(n.points.out$unit.points)
  #get n.points (unit.points x nVisits)
  n.points.out$n.points<-n.points.out$unit.points*nVisits

  #total points within refuge
  npoints<-sum(n.points.out$n.points)

  ############################################
  #create folders to save results
  dir.create(paste(getwd(),"Results","SMI_Full","Power_Bayes",speciesName,sep="/"))
  dir.create(paste(getwd(),"Results","SMI_Full","Power_Bayes",speciesName,refugeName,sep="/"))
  dir.create(paste(getwd(),"Results","SMI_Full","Power_Bayes",speciesName,refugeName,paste("SMI_Full_","Power","_", speciesName,"_",refugeName,"_",declineRate, "decline_",nyrs,"years_",nvisits,"visits" ,sep=''),sep="/"))

  ############################################
  #Begin simulation loop
  for (i in 1:nsim) {
    print(i)

    #########################################################################################
    #simulate data
    #dat<-dataSimUnitsFull(dataIn=new.data,modIn=mod.1,declineRate=declineRate,nyrs=nyrs,nvisits=nVisits)
    # dat<-dsim.fun(dataIn=new.data,modIn=mod.1,declineRate=declineRate,nyrs=nyrs,nvisits=nVisits)
    # dat.3<-dsim.fun.3(dataIn=new.data,modIn=mod.1,declineRate=declineRate,nyrs=nyrs,nvisits=nVisits)
    dat<-dataSimUnitsFull(dataIn=new.data,modIn=mod.1,declineRate=declineRate,nyrs=nyrs,nvisits=nVisits)

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
    nc <- 3   ;   ni <- 5000  ;   nb <- 1000  ;   nt <- 20 #ni=30000, nb=5000, nt=10

    ############################################
    #run with 'rjags'
    mod<-jags.model("JAGS.model.unit.txt", data, inits, n.chains = nc, n.adapt=500) #500
    out<-coda.samples(mod, parameters,thin=nt, n.iter=ni)
    res<-summary(window(out, start=1001))
    out$value$model

    if(is.numeric(try(max(gelman.diag(out)[[1]][,1]), silent=T))) Rmax<-max(gelman.diag(out)[[1]][,1]) else Rmax<-NA

    saveFilepath<-paste(getwd(),"Results","SMI_Full","Power_Bayes",speciesName,refugeName,paste("SMI_Full_", "Power","_",speciesName,"_",refugeName ,"_",declineRate, "decline_",nyrs,"years_",nvisits,"visits", sep=''),sep="/")

    dput( list(data=dat, model=res,R=Rmax, mcs=out),
          file=paste(saveFilepath, paste("JAGS_",speciesName,"_",refugeName,"_", npoints, "_", declineRate, "decline_", nyrs,"years_",nvisits,"visits_",i,".R", sep=''),sep="/")   )

  }  #end of First sim loop

  #########################################################################################

  #now harvest results
  message("Now compiling and saving simulation results.")

  #get number of unique observers (use if Observer is det. covariate in model)
  #obsCount<-length(unique(new.data$Observer))-1

  #get coefficients from mod.1 (run previously in 'unmarked')
  # get coefficient estimates to be used in data simulation
  beta<-coef(mod.1)
  beta.names<-names(beta)

  # try(beta0<-coef(mod.1)[1])
  # try(beta1<-coef(mod.1)[1]+coef(mod.1)[2])
  # try(beta2<-coef(mod.1)[1]+coef(mod.1)[3])
  # try(beta3<-coef(mod.1)[1]+coef(mod.1)[4])
  # try(beta4<-coef(mod.1)[1]+coef(mod.1)[5])
  # try(beta5<-coef(mod.1)[1]+coef(mod.1)[6])
  # try(beta6<-coef(mod.1)[1]+coef(mod.1)[7])
  # try(beta7<-coef(mod.1)[1]+coef(mod.1)[8])
  # try(beta8<-coef(mod.1)[1]+coef(mod.1)[9])
  # try(beta9<-coef(mod.1)[1]+coef(mod.1)[10])
  # try(beta10<-coef(mod.1)[1]+coef(mod.1)[11])
  # try(beta11<-coef(mod.1)[1]+coef(mod.1)[12])
  # try(beta12<-coef(mod.1)[1]+coef(mod.1)[13])
  # try(beta13<-coef(mod.1)[1]+coef(mod.1)[14])
  # try(beta14<-coef(mod.1)[1]+coef(mod.1)[15])
  # try(beta15<-coef(mod.1)[1]+coef(mod.1)[16])
  # try(beta16<-coef(mod.1)[1]+coef(mod.1)[17])
  # try(beta17<-coef(mod.1)[1]+coef(mod.1)[18])
  # try(beta18<-coef(mod.1)[1]+coef(mod.1)[19])
  # try(beta19<-coef(mod.1)[1]+coef(mod.1)[20])
  # try(beta20<-coef(mod.1)[1]+coef(mod.1)[21])
  # try(beta21<-coef(mod.1)[1]+coef(mod.1)[22])
  # try(beta22<-coef(mod.1)[1]+coef(mod.1)[23])
  # try(beta23<-coef(mod.1)[1]+coef(mod.1)[24])
  # try(beta24<-coef(mod.1)[1]+coef(mod.1)[25])
  # try(beta25<-coef(mod.1)[1]+coef(mod.1)[26])
  #
  # coefs<-if(length(unitNumList)==1){
  #   c(beta0, beta[(length(unitNumList)+1):(length(unitNumList)+4)])
  # }else{if(length(unitNumList)==2){
  #   c(beta0, beta1, beta[(length(unitNumList)+1):(length(unitNumList)+4)])
  # }else{if(length(unitNumList)==3){
  #   c(beta0, beta1, beta2, beta[(length(unitNumList)+1):(length(unitNumList)+4)])
  # }else{if(length(unitNumList)==4){
  #   c(beta0, beta1, beta2,beta3, beta[(length(unitNumList)+1):(length(unitNumList)+4)])
  # }else{if(length(unitNumList)==5){
  #   c(beta0, beta1, beta2,beta3,beta4, beta[(length(unitNumList)+1):(length(unitNumList)+4)])
  # }else{if(length(unitNumList)==6){
  #   c(beta0, beta1, beta2,beta3,beta4,beta5, beta[(length(unitNumList)+1):(length(unitNumList)+4)])
  # }else{if(length(unitNumList)==7){
  #   c(beta0, beta1, beta2,beta3,beta4,beta5,beta6, beta[(length(unitNumList)+1):(length(unitNumList)+4)])
  # }else{if(length(unitNumList)==8){
  #   c(beta0, beta1, beta2,beta3,beta4,beta5,beta6,beta7, beta[(length(unitNumList)+1):(length(unitNumList)+4)])
  # }else{if(length(unitNumList)==9){
  #   c(beta0, beta1, beta2,beta3,beta4,beta5,beta6,beta7,beta8, beta[(length(unitNumList)+1):(length(unitNumList)+4)])
  # }else{if(length(unitNumList)==10){
  #   c(beta0, beta1, beta2,beta3,beta4,beta5,beta6,beta7, beta8,beta9, beta[(length(unitNumList)+1):(length(unitNumList)+4)])
  # }else{if(length(unitNumList)==11){
  #   c(beta0, beta1, beta2,beta3,beta4,beta5,beta6,beta7, beta8,beta9,beta10, beta[(length(unitNumList)+1):(length(unitNumList)+4)])
  # }else{if(length(unitNumList)==12){
  #   c(beta0, beta1, beta2,beta3,beta4,beta5,beta6,beta7, beta8,beta9,beta10,beta11, beta[(length(unitNumList)+1):(length(unitNumList)+4)])
  # }else{if(length(unitNumList)==13){
  #   c(beta0, beta1, beta2,beta3,beta4,beta5,beta6,beta7, beta8,beta9,beta10,beta11,beta12, beta[(length(unitNumList)+1):(length(unitNumList)+4)])
  # }else{if(length(unitNumList)==14){
  #   c(beta0, beta1, beta2,beta3,beta4,beta5,beta6,beta7, beta8,beta9,beta10,beta11,beta12,beta13, beta[(length(unitNumList)+1):(length(unitNumList)+4)])
  # }else{if(length(unitNumList)==15){
  #   c(beta0, beta1, beta2,beta3,beta4,beta5,beta6,beta7, beta8,beta9,beta10,beta11,beta12,beta13,beta14, beta[(length(unitNumList)+1):(length(unitNumList)+4)])
  # }else{if(length(unitNumList)==16){
  #   c(beta0, beta1, beta2,beta3,beta4,beta5,beta6,beta7, beta8,beta9,beta10,beta11,beta12,beta13,beta14,beta15, beta[(length(unitNumList)+1):(length(unitNumList)+4)])
  # }else{if(length(unitNumList)==17){
  #   c(beta0, beta1, beta2,beta3,beta4,beta5,beta6,beta7, beta8,beta9,beta10,beta11,beta12,beta13,beta14,beta15,beta16, beta[(length(unitNumList)+1):(length(unitNumList)+4)])
  # }else{if(length(unitNumList)==18){
  #   c(beta0, beta1, beta2,beta3,beta4,beta5,beta6,beta7, beta8,beta9,beta10,beta11,beta12,beta13,beta14,beta15,beta16,beta17, beta[(length(unitNumList)+1):(length(unitNumList)+4)])
  # }else{if(length(unitNumList)==19){
  #   c(beta0, beta1, beta2,beta3,beta4,beta5,beta6,beta7, beta8,beta9,beta10,beta11,beta12,beta13,beta14,beta15,beta16,beta17,beta18, beta[(length(unitNumList)+1):(length(unitNumList)+4)])
  # }else{if(length(unitNumList)==20){
  #   c(beta0, beta1, beta2,beta3,beta4,beta5,beta6,beta7, beta8,beta9,beta10,beta11,beta12,beta13,beta14,beta15,beta16,beta17,beta18,beta19, beta[(length(unitNumList)+1):(length(unitNumList)+4)])
  # }else{if(length(unitNumList)==21){
  #   c(beta0, beta1, beta2,beta3,beta4,beta5,beta6,beta7, beta8,beta9,beta10,beta11,beta12,beta13,beta14,beta15,beta16,beta17,beta18,beta19,beta20, beta[(length(unitNumList)+1):(length(unitNumList)+4)])
  # }else{if(length(unitNumList)==22){
  #   c(beta0, beta1, beta2,beta3,beta4,beta5,beta6,beta7, beta8,beta9,beta10,beta11,beta12,beta13,beta14,beta15,beta16,beta17,beta18,beta19,beta20,beta21, beta[(length(unitNumList)+1):(length(unitNumList)+4)])
  # }else{if(length(unitNumList)==23){
  #   c(beta0, beta1, beta2,beta3,beta4,beta5,beta6,beta7, beta8,beta9,beta10,beta11,beta12,beta13,beta14,beta15,beta16,beta17,beta18,beta19,beta20,beta21,beta22, beta[(length(unitNumList)+1):(length(unitNumList)+4)])
  # }else{if(length(unitNumList)==24){
  #   c(beta0, beta1, beta2,beta3,beta4,beta5,beta6,beta7, beta8,beta9,beta10,beta11,beta12,beta13,beta14,beta15,beta16,beta17,beta18,beta19,beta20,beta21,beta22,beta23, beta[(length(unitNumList)+1):(length(unitNumList)+4)])
  # }else{if(length(unitNumList)==25){
  #   c(beta0, beta1, beta2,beta3,beta4,beta5,beta6,beta7, beta8,beta9,beta10,beta11,beta12,beta13,beta14,beta15,beta16,beta17,beta18,beta19,beta20,beta21,beta22,beta23,beta24, beta[(length(unitNumList)+1):(length(unitNumList)+4)])
  # }}}}}}}}}}}}}}}}}}}}}}}}}
  # names(coefs)<-beta.names
  #########################################################################################
  #get multinomial detection probs

  #for model with effects of Visit on detection probability
  det.probs<-getP(mod.1)

  ##########################################################################################################################################
  #set declineRate
  surv<-switch(declineRate,
               "-0.01"= 0.6900498,
               "-0.02"= 0.6801987,
               "-0.05"= 0.6512294,
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
              "-0.01"=  0.30,
              "-0.02"=  0.30,
              "-0.05"=  0.30,
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
  for (i in 1:length(unitNumList)){
    new.data.2<-new.data
    sub.data<-subset(new.data.2, unitNum==unitNumList[i])
    unitName<-unique(as.character(sub.data$SMI_Unit))
    unit.points<-length(unique(sub.data$PointID))
    output<-data.frame(unitName=unitName,unitNum=unitNumList[i], unit.points=unit.points)
    n.points.out<-rbind(n.points.out, output)
  }

  n.points.for.units<-n.points.out[,c("unitName","unit.points")]

  #get proportion of points per refuge
  n.points.out$Proportion<-n.points.out$unit.points/sum(n.points.out$unit.points)
  #get n.points (unit.points x nVisits)
  n.points.out$n.points<-n.points.out$unit.points*nVisits

  #total points within refuge
  npoints<-sum(n.points.out$n.points)


  # get coefficient estimates used in data simulation
  #beta<-coefs  #add column for unitNum
  new.data$SMI_Unit<-as.factor(as.character(new.data$SMI_Unit))

  new.data$unitNum<-as.integer(new.data$SMI_Unit)

  unitNumList<-sort(unique(new.data$unitNum))


  #filepath for JAGS model no CAR
  saveFilepath<-paste(getwd(),"Results","SMI_Full","Power_Bayes",speciesName,refugeName,paste("SMI_Full_", "Power","_",speciesName,"_",refugeName ,"_",declineRate, "decline_",nyrs,"years_",nvisits,"visits", sep=''),sep="/")

  str<-c(paste(saveFilepath, paste("JAGS_", speciesName,"_",refugeName,"_", npoints, "_", declineRate, "decline_", nyrs,"years_",nvisits,"visits_",1:nsim,".R", sep=''),sep="/"))

  #modify based on nvisits

  #truebeta<-c(beta[1:nUnits],rep(Beta,nUnits),beta[nUnits+1], beta[nUnits+4])
  truebeta<-c(beta[1],beta[5:(nUnits+3)],rep(Beta,nUnits),beta[nUnits+4], beta[nUnits+7])

  #BetaRes<-array(0, c(nsim, 6, (nUnits*2+2)))
  BetaRes<-array(0, c(nsim, 6, (nUnits*2+3)))

  new.betaList<-list()
  betaList<-for(i in 1:nUnits){
    new.betaList[[i]]<-c(paste("beta2","[",i,"]",sep=""))
  }

  new.gammaList<-list()
  gammaList<-for(i in 1:nUnits){
    new.gammaList[[i]]<-c(paste("gamma","[",i,"]",sep=""))
  }

  # dimnames(BetaRes)<-list((1:nsim), c('Mean','p', 'Bias','Lower','Upper', 'Coverage'),
  #                         c(new.betaList,
  #                           new.gammaList,
  #                           "p(Int)", "alpha(alpha)") )

  dimnames(BetaRes)<-list((1:nsim), c('Mean','p', 'Bias','Lower','Upper', 'Coverage'),
                          c("lambda(Int)",
                            new.betaList,
                            new.gammaList,
                            "p(Int)", "alpha(alpha)") )



  for(i in 1:nsim){

    print(i)

    out<-source(str[i])
    mod<-out$value$model

    coefsJAGS<-mod$statistics[,1]
    #ord<-c(seq(3+nvisits,(nUnits*2+3+nvisits)),1)
    ord<-c(seq(5,(nUnits*2+6)),1)
    CI<-mod$quantiles[ord,c(1,5)]

    #coef values
    BetaRes[i,1,]<-coefsJAGS[ord]

    #bias
    BetaRes[i,3,]<-(coefsJAGS[ord]-truebeta)/truebeta

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
  #beta.names<-names(beta)[1:nUnits]
  beta.names<-names(beta)[c(1,5:(nUnits+3))]
  beta.names.1<-data.frame(beta.names=beta.names)
  beta.names.1$beta.names<-as.character(beta.names.1$beta.names)
  beta.names.2<-gsub("lambda[(]", "", beta.names.1$beta.names)
  beta.names.2<-gsub(")", "", beta.names.2)
  beta.names.3<-gsub("unitNum", "", beta.names.2)
  beta.names.out<-data.frame(unitNum=beta.names.3)
  beta.names.out$unitNum<-as.integer(beta.names.out$unitNum)

  #get unit table
  ref.table<-unique(new.data[,c("unitNum","SMI_Unit")])

  ref.table.1<-merge(beta.names.out,ref.table, by=c("unitNum"),all=TRUE)
  ref.table.1$unitNum<-as.character(ref.table.1$unitNum)
  ref.table.out<-data.frame(SMI_Unit=ref.table.1$SMI_Unit, unitNum=ref.table.1$unitNum,numPoints=n.points.for.units[,2])

  #format results as table
  #abun.est<-Restab[1:nUnits,]
  abun.est<-Restab[2:(nUnits+1),]
  abun.est$Predicted<-exp(abun.est$Mean)
  abun.est.1<-cbind(ref.table.out, abun.est)
  abun.est.out<-abun.est.1[,c("SMI_Unit","unitNum","numPoints","Mean","rmse","BiasMean","Predicted")]
  #save abundance estimates
  write.csv(abun.est.out, file=paste(saveFilepath, "abun.out.csv",sep="/"),row.names=TRUE)

  #power.table<-Restab[(nUnits+1):(nUnits*2),]
  power.table<-Restab[(nUnits+2):(nUnits*2+1),]
  power.table$model_Type<-paste("SMI_Full","_Power_Bayes","_",speciesName,"_", refugeName,"_",npoints,"_", round(Beta, dig=2), "decline_", nyrs,"_years", sep='')
  power.table$NumPoints<-abun.est.out$numPoints
  power.table$nVisits<-nvisits
  power.table.1<-data.frame(power.table,abun.est.out[,c("SMI_Unit","unitNum","Predicted")])
  power.table.out<-power.table.1[,c("SMI_Unit","unitNum","NumPoints","nVisits","Predicted","Mean","rmse","BiasMean","CI","significance","pwr","model_Type")]
  colnames(power.table.out)[5]<-"Predicted.abun"
  colnames(power.table.out)[6]<-"Mean_gamma"
  #save results

  write.csv(Restab, file=paste(saveFilepath, "summary.output.csv",sep="/"),row.names=TRUE)

  write.csv(power.table.out, file=paste(saveFilepath, "power.table.out.csv",sep="/"),row.names=TRUE)

}

