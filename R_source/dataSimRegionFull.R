#function to simulate new data based on beta coeficients from 'unmarked' model
#data simulate Full Panel
dataSimRegionFull<-function(dataIn,modIn,declineRate,nyrs,nvisits){

  #read in data
  new.data<-dataIn

  #set number of repeat visits
  nVisits=nvisits

  #add column for refugeNum
  new.data$NWR_code<-as.factor(as.character(new.data$NWR_code))

  new.data$refugeNum<-as.integer(new.data$NWR_code)

  refugeNumList<-sort(unique(new.data$refugeNum))

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

  mod.1<-modIn

  sim.N.out<-list()
  sim.det.out<-list()
  sim.refuge.out<-list()
  sim.Visits.out<-list()
  sim.Covs.out<-list()
  sim.cell.out<-list()

  for(q in 1:length(refugeNumList)){
    #get n.points for each refuge
    ref.npoints<-n.points.out$n.points[q]

    # get coefficient estimates to be used in data simulation
    beta<-coef(mod.all)
    betaBIRD<-if(q==1){
      beta[q]
    }else{if(q==2){
      beta[q-1]+beta[q]
    }else{if(q==3){
      beta[q-2]+beta[q]
    }else{if(q==4){
      beta[q-3]+beta[q]
    }else{if(q==5){
      beta[q-4]+beta[q]
    }}}}}

    #predict expected abundance per point count on log-scale
    Xspr<-cbind(rep(1,ref.npoints))
    lam<-Xspr%*%(betaBIRD)

    #get parameters for detection function
    dparm<-beta[c("p(Int)")]
    sigma<-exp(Xspr%*%dparm)

    #number of sampling points (to simulate data for)
    J<-ref.npoints

    lamnew<-exp(lam)

    #modify survival and recruitment rates (arbirtary) to achieve desired rate of decline
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

    #number of sampling points
    #ref.npoints<- J #modify for testing scenarios with varying number of survey locations

    #J=dim(lamnew)[1] #number of grid cells

    cell.1<-sort(sample(1:J, ref.npoints, replace=FALSE))

    Nsim<-matrix(nrow=ref.npoints, ncol=nyrs)

    alpha<-beta[("alpha(alpha)")]

    #yr 1 as before
    Nsim[,1]<-rnbinom(n=ref.npoints, size=exp(alpha), mu=lamnew[cell.1,1]) #generate individual countes per grid cell/point count circle

    for(y in 2:nyrs){
      Nsim[,y]<-rbinom(ref.npoints,Nsim[,y-1], surv) + rpois(ref.npoints,Nsim[,y-1]*rec)
    }

    multinom.probs<-colMeans(unique(getP(mod.1)))

    #make detection data for nyrs
    det.2<-list()
    det.out<-list()
    for(j in 1:nyrs){

      det.1<-list()
      for(i in 1:ref.npoints){
        det.Y1<-t(rmultinom(1,size=Nsim[i,j], prob=multinom.probs))
        det.1<-rbind(det.1, det.Y1)
      }
      det.2[[j]]<-data.frame(matrix(as.numeric(det.1),ncol=5))
      det.out<-det.2
    }

    detList<-det.out

  #get list of Visits (nVisits)
  Visits<-data.frame(VisitNum=rep(seq(1:nVisits),(ref.npoints/nVisits)))

  #get vector of refuge_IDs  * nVisits
  refuge.output<-data.frame(refugeNum=rep(refugeNumList[q],ref.npoints))

  sim.refuge.out<-rbind(sim.refuge.out, refuge.output)

  sim.Visits.out<-rbind(sim.Visits.out, Visits)

  covList<-data.frame(sim.refuge.out, sim.Visits.out)

  site.covs<-data.frame(rep(covList, nyrs))

  sim.N.out<-rbind(sim.N.out, Nsim )
  sim.det.out<-append(sim.det.out, detList)
  sim.Covs.out<-rbind(sim.Covs.out, covList)
}

new.sim.det.out<-list()
new.out<-list()
for(v in 1:nyrs){

  new.out<-if(length(refugeNumList)==1){
        rbind(as.data.frame(sim.det.out[v]))
      }else {if(length(refugeNumList)==2){
        rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+5]))
      } else {if(length(refugeNumList)==3){
        rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+5]),as.data.frame(sim.det.out[v+10]))
      } else {if(length(refugeNumList)==4){
        rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+5]),as.data.frame(sim.det.out[v+10]),as.data.frame(sim.det.out[v+15]) )
      } else {if(length(refugeNumList)==5){
        rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+5]),as.data.frame(sim.det.out[v+10]),as.data.frame(sim.det.out[v+15]),as.data.frame(sim.det.out[v+20]) )
      }}}}}

  new.sim.det.out[[v]]<-new.out

}

cell.2=sort(seq(1:npoints))

  return(list(detList=new.sim.det.out, covList=sim.Covs.out, N=sim.N.out, cell=cell.2))

}#end function
