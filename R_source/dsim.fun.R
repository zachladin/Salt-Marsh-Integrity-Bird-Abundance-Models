#function to simulate new data based on beta coeficients from 'unmarked' model
dsim.fun<-function(dataIn,modIn,declineRate,nyrs,nvisits){

  #read in data
  new.data<-dataIn

  #set number of repeat visits
  nVisits=nvisits

  unitNumList<-sort(unique(new.data$unitNum))

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

  #total points within refuge
  npoints<-sum(n.points.out$n.points)

  mod.1<-modIn

  # get coefficient estimates to be used in data simulation
  beta<-coef(mod.1)
  betaBIRD<-beta[c("lambda(Int)")]

  #predict expected abundance per point count on log-scale
  Xspr<-cbind(rep(1,npoints))
  lam<-Xspr%*%(betaBIRD)
  #predict(mod.1, type="lambda")

  #get parameters for detection function
  dparm<-beta[c("p(Int)")]
  sigma<-exp(Xspr%*%dparm)

  #number of sampling points (to simulate data for)
  J<-npoints

  lamnew<-exp(lam)

  #FOR 50% decline: change surv to 0.5065  and rec to 0.1 for pop. rate of change of -0.50
  #FOR 25% decline: change surv to 0.5785  and rec to 0.2 for pop. rate of change of -0.25
  #FOR 10% decline: change surv to 0.6045  and rec to 0.3 for pop. rate of change of -0.10

  surv<-switch(declineRate,
               "-0.10"= 0.6045,
               "-0.25"= 0.5785,
               "-0.50"= 0.5065)

  rec<-switch(declineRate,
              "-0.10" = 0.3,
              "-0.25" = 0.2,
              "-0.50" = 0.1)

  Beta<- log(surv+rec)

  #number of sampling points
  npoints<- J #modify for testing scenarios with varying number of survey locations

  J=dim(lamnew)[1] #number of grid cells

  cell<-sort(sample(1:J, npoints, replace=FALSE))

  Nsim<-matrix(nrow=npoints, ncol=nyrs)

  alpha<-beta[("alpha(alpha)")]

  #yr 1 as before
  Nsim[,1]<-rnbinom(n=npoints, size=exp(alpha), mu=lamnew[cell,1]) #generate individual countes per grid cell/point count circle

  for(y in 2:nyrs){
    Nsim[,y]<-rbinom(npoints,Nsim[,y-1], surv) + rpois(npoints,Nsim[,y-1]*rec)
  }

  multinom.probs<-colMeans(unique(getP(mod.1)))

  #make detection data for nyrs
  det.2<-list()
  det.out<-list()
  for(j in 1:nyrs){

    det.1<-list()
    for(i in 1:npoints){
      det.Y1<-t(rmultinom(1,size=Nsim[i,j], prob=multinom.probs))
      det.1<-rbind(det.1, det.Y1)
    }
    det.2[[j]]<-matrix(as.numeric(det.1),ncol=5)
    det.out<-det.2
  }

  detList<-det.out

  #get list of Visits (nVisits)
  Visits<-data.frame(VisitNum=rep(seq(1:nVisits),(npoints/nVisits)))

  #get vector of unit_IDs  * nVisits
  unit.output<-list()
  for(i in 1:length(unitNumList)){
    unit.sub<-subset(n.points.out, unitNum==unitNumList[i])
    n.points.unit<-unit.sub$unit.points*nVisits
    UnitsColumn<-data.frame(unitNum=rep(unit.sub$unitNum,n.points.unit))
    unit.output<-rbind(unit.output,UnitsColumn)
  }

  covList<-data.frame(unit.output, Visits)

  site.covs<-data.frame(rep(covList, nyrs))

  return(list(detList=detList, covList=covList, N=Nsim, cell=cell))

}#end function
