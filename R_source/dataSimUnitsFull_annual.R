#function to simulate new data based on beta coeficients from 'unmarked' model
#data simulate Full Panel
dataSimUnitsFull_annual<-function(dataIn,modIn,declineRate,nyrs,nvisits){

  #read in data
  new.data<-dataIn

  #set number of repeat visits
  nVisits=nvisits

  #get nYears from data
  nYears<-length(unique(new.data$Year))

  #add column for unitNum
  #new.data$SMI_Unit<-as.factor(as.character(new.data$SMI_Unit))

  #new.data$unitNum<-as.integer(new.data$SMI_Unit)

  unitNumList<-sort(unique(new.data$unitNum))

  #get number of points per unit
  n.points.out<-list()
  for (i in 1:length(unitNumList)){
    new.data.2<-new.data
    sub.data<-subset(new.data.2, unitNum==unitNumList[i])
    unit.points<-length(unique(sub.data$PointID))
    output<-data.frame(unitNum=unitNumList[i], unit.points=unit.points)
    n.points.out<-rbind(n.points.out, output)
  }

  #get proportion of points per unit
  n.points.out$Proportion<-n.points.out$unit.points/sum(n.points.out$unit.points)
  #get n.points (unit.points x nVisits)
  n.points.out$n.points<-n.points.out$unit.points*nVisits

  #total points within units
  npoints<-sum(n.points.out$n.points)

  mod.1<-modIn

  # # get coefficient estimates to be used in data simulation
  # beta<-coef(mod.1)
  # betaBIRD<-beta[c("lambda(Int)")]


  sim.N.out<-list()
  sim.det.out<-list()
  sim.unit.out<-list()
  sim.Visits.out<-list()
  sim.Covs.out<-list()
  sim.cell.out<-list()

   for(q in 1:length(unitNumList)){
     #get n.points for each refuge
      unit.npoints<-n.points.out$n.points[q]

    # get coefficient estimates to be used in data simulation
    beta<-coef(mod.1)

    # betaBIRD<-if(q==1){
    #   beta[c("lambda(Int)")]
    # }else{beta[q-1+nYears]}

    #####
    betaBIRD<-if(q==1){
      beta[1]
    }else{
      beta[1]+beta[q]
    }

    #predict expected abundance per point count on log-scale
    Xspr<-cbind(rep(1,unit.npoints))
    lam<-Xspr%*%(betaBIRD)

    #get parameters for detection function
    dparm<-beta[c("p(Int)")]
    sigma<-exp(Xspr%*%dparm)

    #number of sampling points (to simulate data for)
    J<-unit.npoints

    lamnew<-exp(lam)

    #modify survival and recruitment rates (arbirtary) to achieve desired rate of decline
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
    #number of sampling points
    #unit.npoints<- J #modify for testing scenarios with varying number of survey locations

    #J=dim(lamnew)[1] #number of grid cells

    cell.1<-sort(sample(1:J, unit.npoints, replace=FALSE))

    Nsim<-matrix(nrow=unit.npoints, ncol=nyrs)

    alpha<-beta[("alpha(alpha)")]

    #yr 1 as before
    Nsim[,1]<-rnbinom(n=unit.npoints, size=exp(alpha), mu=lamnew[cell.1,1]) #generate individual countes per grid cell/point count circle

    for(y in 2:nyrs){
      Nsim[,y]<-rbinom(unit.npoints,Nsim[,y-1], surv) + rpois(unit.npoints,Nsim[,y-1]*rec)
    }

    multinom.probs<-colMeans(unique(getP(mod.1)))

    #make detection data for nyrs
    det.2<-list()
    det.out<-list()
    for(j in 1:nyrs){

      det.1<-list()
      for(i in 1:unit.npoints){
        det.Y1<-t(rmultinom(1,size=Nsim[i,j], prob=multinom.probs))
        det.1<-rbind(det.1, det.Y1)
      }
      det.2[[j]]<-data.frame(matrix(as.numeric(det.1),ncol=5))
      det.out<-det.2
    }

    #compile detection list
    sim.det.out<-append(sim.det.out, det.out)

    #get list of Visits (nVisits)
    Visits<-data.frame(VisitNum=rep(seq(1:nVisits),(unit.npoints/nVisits)))

    #get vector of unituge_IDs  * nVisits
    unit.output<-data.frame(unitNum=rep(unitNumList[q],unit.npoints))

    sim.unit.out<-rbind(sim.unit.out, unit.output)

    sim.Visits.out<-rbind(sim.Visits.out, Visits)

    sim.N.out<-rbind(sim.N.out, Nsim )
   }

  covList<-data.frame(sim.unit.out, sim.Visits.out)

  #sim.det.out.2<-matrix(unlist(sim.det.out),ncol=nyrs)
  sim.N.out.2<-matrix(unlist(sim.N.out),ncol=nyrs)

  test.3<-list()
  for(v in 1:nyrs){
  test<-rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+nyrs]),as.data.frame(sim.det.out[v+nyrs*2]),as.data.frame(sim.det.out[v+nyrs*3]),as.data.frame(sim.det.out[v+nyrs*4]) ,as.data.frame(sim.det.out[v+nyrs*5]),as.data.frame(sim.det.out[v+nyrs*6]) ,as.data.frame(sim.det.out[v+nyrs*7]))
  test.2<-matrix(unlist(test),ncol=5)
  test.3[[v]]<-test.2
  }

  test<-rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+10]),as.data.frame(sim.det.out[v+20]),as.data.frame(sim.det.out[v+30]),as.data.frame(sim.det.out[v+40]) ,as.data.frame(sim.det.out[v+50]),as.data.frame(sim.det.out[v+60]) ,as.data.frame(sim.det.out[v+70]))
  head(sim.det.out)


  det.out.3<-list()
  for(v in 1:nyrs){

  new.out<-if(length(unitNumList)==1){
        rbind(as.data.frame(sim.det.out[v]))
      }else {if(length(unitNumList)==2){
        rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+nyrs]))
      } else {if(length(unitNumList)==3){
        rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+nyrs]),as.data.frame(sim.det.out[v+nyrs*2]))
      } else {if(length(unitNumList)==4){
        rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+nyrs]),as.data.frame(sim.det.out[v+nyrs*2]),as.data.frame(sim.det.out[v+nyrs*3]) )
      } else {if(length(unitNumList)==5){
        rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+nyrs]),as.data.frame(sim.det.out[v+nyrs*2]),as.data.frame(sim.det.out[v+nyrs*3]),as.data.frame(sim.det.out[v+nyrs*4]) )
      }else {if(length(unitNumList)==6){
        rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+nyrs]),as.data.frame(sim.det.out[v+nyrs*2]),as.data.frame(sim.det.out[v+nyrs*3]),as.data.frame(sim.det.out[v+nyrs*4]) ,as.data.frame(sim.det.out[v+nyrs*5]))
      }else {if(length(unitNumList)==7){
        rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+nyrs]),as.data.frame(sim.det.out[v+nyrs*2]),as.data.frame(sim.det.out[v+nyrs*3]),as.data.frame(sim.det.out[v+nyrs*4]),as.data.frame(sim.det.out[v+nyrs*5]) ,as.data.frame(sim.det.out[v+nyrs*6]))
      }else {if(length(unitNumList)==8){
        rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+nyrs]),as.data.frame(sim.det.out[v+nyrs*2]),as.data.frame(sim.det.out[v+nyrs*3]),as.data.frame(sim.det.out[v+nyrs*4]) ,as.data.frame(sim.det.out[v+nyrs*5]),as.data.frame(sim.det.out[v+nyrs*6]) ,as.data.frame(sim.det.out[v+nyrs*7]))
      }else {if(length(unitNumList)==9){
        rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+nyrs]),as.data.frame(sim.det.out[v+nyrs*2]),as.data.frame(sim.det.out[v+nyrs*3]),as.data.frame(sim.det.out[v+nyrs*4]),as.data.frame(sim.det.out[v+nyrs*5]) ,as.data.frame(sim.det.out[v+nyrs*6]),as.data.frame(sim.det.out[v+nyrs*7]) ,as.data.frame(sim.det.out[v+nyrs*8]))
      }else {if(length(unitNumList)==10){
        rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+nyrs]),as.data.frame(sim.det.out[v+nyrs*2]),as.data.frame(sim.det.out[v+nyrs*3]),as.data.frame(sim.det.out[v+nyrs*4]),as.data.frame(sim.det.out[v+nyrs*5]),as.data.frame(sim.det.out[v+nyrs*6]) ,as.data.frame(sim.det.out[v+nyrs*7]),as.data.frame(sim.det.out[v+nyrs*8]) ,as.data.frame(sim.det.out[v+nyrs*9]))
      }else {if(length(unitNumList)==11){
        rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+nyrs]),as.data.frame(sim.det.out[v+nyrs*2]),as.data.frame(sim.det.out[v+nyrs*3]),as.data.frame(sim.det.out[v+nyrs*4]),
              as.data.frame(sim.det.out[v+nyrs*5]) ,as.data.frame(sim.det.out[v+nyrs*6]) ,as.data.frame(sim.det.out[v+nyrs*7]),as.data.frame(sim.det.out[v+nyrs*8]) ,as.data.frame(sim.det.out[v+nyrs*9]),as.data.frame(sim.det.out[v+nyrs0]))
      }else {if(length(unitNumList)==12){
        rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+nyrs]),as.data.frame(sim.det.out[v+nyrs*2]),as.data.frame(sim.det.out[v+nyrs*3]),as.data.frame(sim.det.out[v+nyrs*4]),
              as.data.frame(sim.det.out[v+nyrs*5]) ,as.data.frame(sim.det.out[v+nyrs*6]) ,as.data.frame(sim.det.out[v+nyrs*7]),as.data.frame(sim.det.out[v+nyrs*8]) ,as.data.frame(sim.det.out[v+nyrs*9]),as.data.frame(sim.det.out[v+nyrs*10]),as.data.frame(sim.det.out[v+nyrs*11]))
      }else {if(length(unitNumList)==13){
        rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+nyrs]),as.data.frame(sim.det.out[v+nyrs*2]),as.data.frame(sim.det.out[v+nyrs*3]),as.data.frame(sim.det.out[v+nyrs*4]) ,
              as.data.frame(sim.det.out[v+nyrs*5]) ,as.data.frame(sim.det.out[v+nyrs*6]) ,as.data.frame(sim.det.out[v+nyrs*7]),as.data.frame(sim.det.out[v+nyrs*8]) ,as.data.frame(sim.det.out[v+nyrs*9]),as.data.frame(sim.det.out[v+nyrs*10]),as.data.frame(sim.det.out[v+nyrs*11]),as.data.frame(sim.det.out[v+nyrs*12]))
      }else {if(length(unitNumList)==14){
        rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+nyrs]),as.data.frame(sim.det.out[v+nyrs*2]),as.data.frame(sim.det.out[v+nyrs*3]),as.data.frame(sim.det.out[v+nyrs*4]) ,
              as.data.frame(sim.det.out[v+nyrs*5]) ,as.data.frame(sim.det.out[v+nyrs*6]) ,as.data.frame(sim.det.out[v+nyrs*7]),as.data.frame(sim.det.out[v+nyrs*8]) ,as.data.frame(sim.det.out[v+nyrs*9]),as.data.frame(sim.det.out[v+nyrs*10]),as.data.frame(sim.det.out[v+nyrs*11]),as.data.frame(sim.det.out[v+nyrs*12]),as.data.frame(sim.det.out[v+nyrs*13]))
      }else {if(length(unitNumList)==15){
        rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+nyrs]),as.data.frame(sim.det.out[v+nyrs*2]),as.data.frame(sim.det.out[v+nyrs*3]),as.data.frame(sim.det.out[v+nyrs*4]) ,
              as.data.frame(sim.det.out[v+nyrs*5]) ,as.data.frame(sim.det.out[v+nyrs*6]) ,as.data.frame(sim.det.out[v+nyrs*7]),as.data.frame(sim.det.out[v+nyrs*8]) ,as.data.frame(sim.det.out[v+nyrs*9]),as.data.frame(sim.det.out[v+nyrs*10]),as.data.frame(sim.det.out[v+nyrs*11]),as.data.frame(sim.det.out[v+nyrs*12]),as.data.frame(sim.det.out[v+nyrs*13]),as.data.frame(sim.det.out[v+nyrs*14]))
      }else {if(length(unitNumList)==16){
        rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+nyrs]),as.data.frame(sim.det.out[v+nyrs*2]),as.data.frame(sim.det.out[v+nyrs*3]),as.data.frame(sim.det.out[v+nyrs*4]) ,
              as.data.frame(sim.det.out[v+nyrs*5]) ,as.data.frame(sim.det.out[v+nyrs*6]) ,as.data.frame(sim.det.out[v+nyrs*7]),as.data.frame(sim.det.out[v+nyrs*8]) ,as.data.frame(sim.det.out[v+nyrs*9]),as.data.frame(sim.det.out[v+nyrs*10]),as.data.frame(sim.det.out[v+nyrs*11]),as.data.frame(sim.det.out[v+nyrs*12]),as.data.frame(sim.det.out[v+nyrs*13]),as.data.frame(sim.det.out[v+nyrs*14]),as.data.frame(sim.det.out[v+nyrs*15]))
      }else {if(length(unitNumList)==17){
        rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+nyrs]),as.data.frame(sim.det.out[v+nyrs*2]),as.data.frame(sim.det.out[v+nyrs*3]),as.data.frame(sim.det.out[v+nyrs*4]) ,
              as.data.frame(sim.det.out[v+nyrs*5]) ,as.data.frame(sim.det.out[v+nyrs*6]) ,as.data.frame(sim.det.out[v+nyrs*7]),as.data.frame(sim.det.out[v+nyrs*8]) ,as.data.frame(sim.det.out[v+nyrs*9]),as.data.frame(sim.det.out[v+nyrs*10]),
              as.data.frame(sim.det.out[v+nyrs*11]),as.data.frame(sim.det.out[v+nyrs*12]),as.data.frame(sim.det.out[v+nyrs*13]),as.data.frame(sim.det.out[v+nyrs*14]),as.data.frame(sim.det.out[v+nyrs*15]),as.data.frame(sim.det.out[v+nyrs*16]))
      }else {if(length(unitNumList)==18){
        rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+nyrs]),as.data.frame(sim.det.out[v+nyrs*2]),as.data.frame(sim.det.out[v+nyrs*3]),as.data.frame(sim.det.out[v+nyrs*4]) ,
              as.data.frame(sim.det.out[v+nyrs*5]) ,as.data.frame(sim.det.out[v+nyrs*6]) ,as.data.frame(sim.det.out[v+nyrs*7]),as.data.frame(sim.det.out[v+nyrs*8]) ,as.data.frame(sim.det.out[v+nyrs*9]),as.data.frame(sim.det.out[v+nyrs*10]),
              as.data.frame(sim.det.out[v+nyrs*11]),as.data.frame(sim.det.out[v+nyrs*12]),as.data.frame(sim.det.out[v+nyrs*13]),as.data.frame(sim.det.out[v+nyrs*14]),as.data.frame(sim.det.out[v+nyrs*15]),as.data.frame(sim.det.out[v+nyrs*16]),as.data.frame(sim.det.out[v+nyrs*17]))
      }else {if(length(unitNumList)==19){
        rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+nyrs]),as.data.frame(sim.det.out[v+nyrs*2]),as.data.frame(sim.det.out[v+nyrs*3]),as.data.frame(sim.det.out[v+nyrs*4]) ,
              as.data.frame(sim.det.out[v+nyrs*5]) ,as.data.frame(sim.det.out[v+nyrs*6]) ,as.data.frame(sim.det.out[v+nyrs*7]),as.data.frame(sim.det.out[v+nyrs*8]) ,as.data.frame(sim.det.out[v+nyrs*9]),as.data.frame(sim.det.out[v+nyrs*10]),
              as.data.frame(sim.det.out[v+nyrs*11]),as.data.frame(sim.det.out[v+nyrs*12]),as.data.frame(sim.det.out[v+nyrs*13]),as.data.frame(sim.det.out[v+nyrs*14]),as.data.frame(sim.det.out[v+nyrs*15]),as.data.frame(sim.det.out[v+nyrs*16]),as.data.frame(sim.det.out[v+nyrs*17]),as.data.frame(sim.det.out[v+nyrs*18]))
      }else {if(length(unitNumList)==20){
        rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+nyrs]),as.data.frame(sim.det.out[v+nyrs*2]),as.data.frame(sim.det.out[v+nyrs*3]),as.data.frame(sim.det.out[v+nyrs*4]) ,
              as.data.frame(sim.det.out[v+nyrs*5]) ,as.data.frame(sim.det.out[v+nyrs*6]) ,as.data.frame(sim.det.out[v+nyrs*7]),as.data.frame(sim.det.out[v+nyrs*8]) ,as.data.frame(sim.det.out[v+nyrs*9]),as.data.frame(sim.det.out[v+nyrs*10]),
              as.data.frame(sim.det.out[v+nyrs*11]),as.data.frame(sim.det.out[v+nyrs*12]),as.data.frame(sim.det.out[v+nyrs*13]),as.data.frame(sim.det.out[v+nyrs*14]),as.data.frame(sim.det.out[v+nyrs*15]),as.data.frame(sim.det.out[v+nyrs*16]),as.data.frame(sim.det.out[v+nyrs*17]),as.data.frame(sim.det.out[v+nyrs*18]),as.data.frame(sim.det.out[v+nyrs*19]))
      }else {if(length(unitNumList)==21){
        rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+nyrs]),as.data.frame(sim.det.out[v+nyrs*2]),as.data.frame(sim.det.out[v+nyrs*3]),as.data.frame(sim.det.out[v+nyrs*4]) ,
              as.data.frame(sim.det.out[v+nyrs*5]) ,as.data.frame(sim.det.out[v+nyrs*6]) ,as.data.frame(sim.det.out[v+nyrs*7]),as.data.frame(sim.det.out[v+nyrs*8]) ,as.data.frame(sim.det.out[v+nyrs*9]),as.data.frame(sim.det.out[v+nyrs*10]),
              as.data.frame(sim.det.out[v+nyrs*11]),as.data.frame(sim.det.out[v+nyrs*12]),as.data.frame(sim.det.out[v+nyrs*13]),as.data.frame(sim.det.out[v+nyrs*14]),as.data.frame(sim.det.out[v+nyrs*15]),as.data.frame(sim.det.out[v+nyrs*16]),as.data.frame(sim.det.out[v+nyrs*17]),as.data.frame(sim.det.out[v+nyrs*18]),as.data.frame(sim.det.out[v+nyrs*19]),as.data.frame(sim.det.out[v+nyrs*20]))
      }else {if(length(unitNumList)==22){
        rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+nyrs]),as.data.frame(sim.det.out[v+nyrs*2]),as.data.frame(sim.det.out[v+nyrs*3]),as.data.frame(sim.det.out[v+nyrs*4]) ,
              as.data.frame(sim.det.out[v+nyrs*5]) ,as.data.frame(sim.det.out[v+nyrs*6]) ,as.data.frame(sim.det.out[v+nyrs*7]),as.data.frame(sim.det.out[v+nyrs*8]) ,as.data.frame(sim.det.out[v+nyrs*9]),as.data.frame(sim.det.out[v+nyrs*10]),
              as.data.frame(sim.det.out[v+nyrs*11]),as.data.frame(sim.det.out[v+nyrs*12]),as.data.frame(sim.det.out[v+nyrs*13]),as.data.frame(sim.det.out[v+nyrs*14]),as.data.frame(sim.det.out[v+nyrs*15]),as.data.frame(sim.det.out[v+nyrs*16]),as.data.frame(sim.det.out[v+nyrs*17]),as.data.frame(sim.det.out[v+nyrs*18]),as.data.frame(sim.det.out[v+nyrs*19]),as.data.frame(sim.det.out[v+nyrs*20]),as.data.frame(sim.det.out[v+nyrs*21]))
      }else {if(length(unitNumList)==23){
        rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+nyrs]),as.data.frame(sim.det.out[v+nyrs*2]),as.data.frame(sim.det.out[v+nyrs*3]),as.data.frame(sim.det.out[v+nyrs*4]) ,
              as.data.frame(sim.det.out[v+nyrs*5]) ,as.data.frame(sim.det.out[v+nyrs*6]) ,as.data.frame(sim.det.out[v+nyrs*7]),as.data.frame(sim.det.out[v+nyrs*8]) ,as.data.frame(sim.det.out[v+nyrs*9]),as.data.frame(sim.det.out[v+nyrs*10]),
              as.data.frame(sim.det.out[v+nyrs*11]),as.data.frame(sim.det.out[v+nyrs*12]),as.data.frame(sim.det.out[v+nyrs*13]),as.data.frame(sim.det.out[v+nyrs*14]),as.data.frame(sim.det.out[v+nyrs*15]),as.data.frame(sim.det.out[v+nyrs*16]),as.data.frame(sim.det.out[v+nyrs*17]),as.data.frame(sim.det.out[v+nyrs*18]),as.data.frame(sim.det.out[v+nyrs*19]),
              as.data.frame(sim.det.out[v+nyrs*20]),as.data.frame(sim.det.out[v+nyrs*21]),as.data.frame(sim.det.out[v+nyrs*22]))
      }else {if(length(unitNumList)==24){
        rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+nyrs]),as.data.frame(sim.det.out[v+nyrs*2]),as.data.frame(sim.det.out[v+nyrs*3]),as.data.frame(sim.det.out[v+nyrs*4]) ,
              as.data.frame(sim.det.out[v+nyrs*5]) ,as.data.frame(sim.det.out[v+nyrs*6]) ,as.data.frame(sim.det.out[v+nyrs*7]),as.data.frame(sim.det.out[v+nyrs*8]) ,as.data.frame(sim.det.out[v+nyrs*9]),as.data.frame(sim.det.out[v+nyrs*10]),
              as.data.frame(sim.det.out[v+nyrs*11]),as.data.frame(sim.det.out[v+nyrs*12]),as.data.frame(sim.det.out[v+nyrs*13]),as.data.frame(sim.det.out[v+nyrs*14]),as.data.frame(sim.det.out[v+nyrs*15]),as.data.frame(sim.det.out[v+nyrs*16]),as.data.frame(sim.det.out[v+nyrs*17]),as.data.frame(sim.det.out[v+nyrs*18]),as.data.frame(sim.det.out[v+nyrs*19]),
              as.data.frame(sim.det.out[v+nyrs*20]),as.data.frame(sim.det.out[v+nyrs*21]),as.data.frame(sim.det.out[v+nyrs*22]),as.data.frame(sim.det.out[v+nyrs*23]))
      }else {if(length(unitNumList)==25){
        rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+nyrs]),as.data.frame(sim.det.out[v+nyrs*2]),as.data.frame(sim.det.out[v+nyrs*3]),as.data.frame(sim.det.out[v+nyrs*4]) ,
              as.data.frame(sim.det.out[v+nyrs*5]) ,as.data.frame(sim.det.out[v+nyrs*6]) ,as.data.frame(sim.det.out[v+nyrs*7]),as.data.frame(sim.det.out[v+nyrs*8]) ,as.data.frame(sim.det.out[v+nyrs*9]),as.data.frame(sim.det.out[v+nyrs*10]),
              as.data.frame(sim.det.out[v+nyrs*11]),as.data.frame(sim.det.out[v+nyrs*12]),as.data.frame(sim.det.out[v+nyrs*13]),as.data.frame(sim.det.out[v+nyrs*14]),as.data.frame(sim.det.out[v+nyrs*15]),as.data.frame(sim.det.out[v+nyrs*16]),as.data.frame(sim.det.out[v+nyrs*17]),as.data.frame(sim.det.out[v+nyrs*18]),as.data.frame(sim.det.out[v+nyrs*19]),
              as.data.frame(sim.det.out[v+nyrs*20]),as.data.frame(sim.det.out[v+nyrs*21]),as.data.frame(sim.det.out[v+nyrs*22]),as.data.frame(sim.det.out[v+nyrs*23]),as.data.frame(sim.det.out[v+nyrs*24]))
      }}}}}}}}}}}}}}}}}}}}}}}}}

  det.2<-matrix(unlist(new.out),ncol=5)
  det.out.3[[v]]<-det.2
  }

cell.2=sort(seq(1:npoints))

  return(list(detList=det.out.3, covList=covList, N=sim.N.out.2, cell=cell.2))

}#end function
