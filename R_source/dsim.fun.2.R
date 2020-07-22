dsim.fun.2<-function(dataIn,modIn,declineRate,nyrs,nvisits){

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

    betaBIRD<-if(q==1){
      beta[c("lambda(Int)")]
    }else{beta[q-1+nYears]}

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

    J=dim(lamnew)[1] #number of grid cells

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

    detList<-det.out

    #get list of Visits (nVisits)
    Visits<-data.frame(VisitNum=rep(seq(1:nVisits),(unit.npoints/nVisits)))

    #get vector of unituge_IDs  * nVisits
    unit.output<-data.frame(unitNum=rep(unitNumList[q],unit.npoints))

    sim.unit.out<-rbind(sim.unit.out, unit.output)

    sim.Visits.out<-rbind(sim.Visits.out, Visits)

    covList<-data.frame(sim.unit.out, sim.Visits.out)

    site.covs<-data.frame(rep(covList, nyrs))

    sim.N.out<-rbind(sim.N.out, Nsim )
    sim.det.out<-append(sim.det.out, detList)
    sim.Covs.out<-rbind(sim.Covs.out, covList)
  }
  apply( cbind( list1, list2 ) , 1 , unlist )
  new.det.test<-rbind(sim.det.out[nyrs],sim.det.out[nyrs+5],sim.det.out[nyrs+10],
                       sim.det.out[nyrs+15],sim.det.out[nyrs+20] ,sim.det.out[nyrs+25],sim.det.out[nyrs+30]
                       ,sim.det.out[nyrs+35])

  new.det.test.2<-Reduce(merge,new.det.test)


  new.sim.det.out<-list()
  new.out<-list()
  for(v in 1:nyrs){

    new.out<-if(length(unitNumList)==1){
      rbind(as.data.frame(sim.det.out[v]))
    }else {if(length(unitNumList)==2){
      rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+5]))
    } else {if(length(unitNumList)==3){
      rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+5]),as.data.frame(sim.det.out[v+10]))
    } else {if(length(unitNumList)==4){
      rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+5]),as.data.frame(sim.det.out[v+10]),as.data.frame(sim.det.out[v+15]) )
    } else {if(length(unitNumList)==5){
      rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+5]),as.data.frame(sim.det.out[v+10]),as.data.frame(sim.det.out[v+15]),as.data.frame(sim.det.out[v+20]) )
    }else {if(length(unitNumList)==6){
      rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+5]),as.data.frame(sim.det.out[v+10]),as.data.frame(sim.det.out[v+15]),as.data.frame(sim.det.out[v+20]) ,as.data.frame(sim.det.out[v+25]))
    }else {if(length(unitNumList)==7){
      rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+5]),as.data.frame(sim.det.out[v+10]),as.data.frame(sim.det.out[v+15]),as.data.frame(sim.det.out[v+20]),as.data.frame(sim.det.out[v+25]) ,as.data.frame(sim.det.out[v+30]))
    }else {if(length(unitNumList)==8){
      rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+5]),as.data.frame(sim.det.out[v+10]),as.data.frame(sim.det.out[v+15]),as.data.frame(sim.det.out[v+20]) ,as.data.frame(sim.det.out[v+25]),as.data.frame(sim.det.out[v+30]) ,as.data.frame(sim.det.out[v+35]))
    }else {if(length(unitNumList)==9){
      rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+5]),as.data.frame(sim.det.out[v+10]),as.data.frame(sim.det.out[v+15]),as.data.frame(sim.det.out[v+20]),as.data.frame(sim.det.out[v+25]) ,as.data.frame(sim.det.out[v+30]),as.data.frame(sim.det.out[v+35]) ,as.data.frame(sim.det.out[v+40]))
    }else {if(length(unitNumList)==10){
      rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+5]),as.data.frame(sim.det.out[v+10]),as.data.frame(sim.det.out[v+15]),as.data.frame(sim.det.out[v+20]),as.data.frame(sim.det.out[v+25]),as.data.frame(sim.det.out[v+30]) ,as.data.frame(sim.det.out[v+35]),as.data.frame(sim.det.out[v+40]) ,as.data.frame(sim.det.out[v+45]))
    }else {if(length(unitNumList)==11){
      rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+5]),as.data.frame(sim.det.out[v+10]),as.data.frame(sim.det.out[v+15]),as.data.frame(sim.det.out[v+20]),
            as.data.frame(sim.det.out[v+25]) ,as.data.frame(sim.det.out[v+30]) ,as.data.frame(sim.det.out[v+35]),as.data.frame(sim.det.out[v+40]) ,as.data.frame(sim.det.out[v+45]),as.data.frame(sim.det.out[v+50]))
    }else {if(length(unitNumList)==12){
      rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+5]),as.data.frame(sim.det.out[v+10]),as.data.frame(sim.det.out[v+15]),as.data.frame(sim.det.out[v+20]),
            as.data.frame(sim.det.out[v+25]) ,as.data.frame(sim.det.out[v+30]) ,as.data.frame(sim.det.out[v+35]),as.data.frame(sim.det.out[v+40]) ,as.data.frame(sim.det.out[v+45]),as.data.frame(sim.det.out[v+50]),as.data.frame(sim.det.out[v+55]))
    }else {if(length(unitNumList)==13){
      rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+5]),as.data.frame(sim.det.out[v+10]),as.data.frame(sim.det.out[v+15]),as.data.frame(sim.det.out[v+20]) ,
            as.data.frame(sim.det.out[v+25]) ,as.data.frame(sim.det.out[v+30]) ,as.data.frame(sim.det.out[v+35]),as.data.frame(sim.det.out[v+40]) ,as.data.frame(sim.det.out[v+45]),as.data.frame(sim.det.out[v+50]),as.data.frame(sim.det.out[v+55]),as.data.frame(sim.det.out[v+60]))
    }else {if(length(unitNumList)==14){
      rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+5]),as.data.frame(sim.det.out[v+10]),as.data.frame(sim.det.out[v+15]),as.data.frame(sim.det.out[v+20]) ,
            as.data.frame(sim.det.out[v+25]) ,as.data.frame(sim.det.out[v+30]) ,as.data.frame(sim.det.out[v+35]),as.data.frame(sim.det.out[v+40]) ,as.data.frame(sim.det.out[v+45]),as.data.frame(sim.det.out[v+50]),as.data.frame(sim.det.out[v+55]),as.data.frame(sim.det.out[v+60]),as.data.frame(sim.det.out[v+65]))
    }else {if(length(unitNumList)==15){
      rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+5]),as.data.frame(sim.det.out[v+10]),as.data.frame(sim.det.out[v+15]),as.data.frame(sim.det.out[v+20]) ,
            as.data.frame(sim.det.out[v+25]) ,as.data.frame(sim.det.out[v+30]) ,as.data.frame(sim.det.out[v+35]),as.data.frame(sim.det.out[v+40]) ,as.data.frame(sim.det.out[v+45]),as.data.frame(sim.det.out[v+50]),as.data.frame(sim.det.out[v+55]),as.data.frame(sim.det.out[v+60]),as.data.frame(sim.det.out[v+65]),as.data.frame(sim.det.out[v+70]))
    }else {if(length(unitNumList)==16){
      rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+5]),as.data.frame(sim.det.out[v+10]),as.data.frame(sim.det.out[v+15]),as.data.frame(sim.det.out[v+20]) ,
            as.data.frame(sim.det.out[v+25]) ,as.data.frame(sim.det.out[v+30]) ,as.data.frame(sim.det.out[v+35]),as.data.frame(sim.det.out[v+40]) ,as.data.frame(sim.det.out[v+45]),as.data.frame(sim.det.out[v+50]),as.data.frame(sim.det.out[v+55]),as.data.frame(sim.det.out[v+60]),as.data.frame(sim.det.out[v+65]),as.data.frame(sim.det.out[v+70]),as.data.frame(sim.det.out[v+75]))
    }else {if(length(unitNumList)==17){
      rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+5]),as.data.frame(sim.det.out[v+10]),as.data.frame(sim.det.out[v+15]),as.data.frame(sim.det.out[v+20]) ,
            as.data.frame(sim.det.out[v+25]) ,as.data.frame(sim.det.out[v+30]) ,as.data.frame(sim.det.out[v+35]),as.data.frame(sim.det.out[v+40]) ,as.data.frame(sim.det.out[v+45]),as.data.frame(sim.det.out[v+50]),
            as.data.frame(sim.det.out[v+55]),as.data.frame(sim.det.out[v+60]),as.data.frame(sim.det.out[v+65]),as.data.frame(sim.det.out[v+70]),as.data.frame(sim.det.out[v+75]),as.data.frame(sim.det.out[v+80]))
    }else {if(length(unitNumList)==18){
      rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+5]),as.data.frame(sim.det.out[v+10]),as.data.frame(sim.det.out[v+15]),as.data.frame(sim.det.out[v+20]) ,
            as.data.frame(sim.det.out[v+25]) ,as.data.frame(sim.det.out[v+30]) ,as.data.frame(sim.det.out[v+35]),as.data.frame(sim.det.out[v+40]) ,as.data.frame(sim.det.out[v+45]),as.data.frame(sim.det.out[v+50]),
            as.data.frame(sim.det.out[v+55]),as.data.frame(sim.det.out[v+60]),as.data.frame(sim.det.out[v+65]),as.data.frame(sim.det.out[v+70]),as.data.frame(sim.det.out[v+75]),as.data.frame(sim.det.out[v+80]),as.data.frame(sim.det.out[v+85]))
    }else {if(length(unitNumList)==19){
      rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+5]),as.data.frame(sim.det.out[v+10]),as.data.frame(sim.det.out[v+15]),as.data.frame(sim.det.out[v+20]) ,
            as.data.frame(sim.det.out[v+25]) ,as.data.frame(sim.det.out[v+30]) ,as.data.frame(sim.det.out[v+35]),as.data.frame(sim.det.out[v+40]) ,as.data.frame(sim.det.out[v+45]),as.data.frame(sim.det.out[v+50]),
            as.data.frame(sim.det.out[v+55]),as.data.frame(sim.det.out[v+60]),as.data.frame(sim.det.out[v+65]),as.data.frame(sim.det.out[v+70]),as.data.frame(sim.det.out[v+75]),as.data.frame(sim.det.out[v+80]),as.data.frame(sim.det.out[v+85]),as.data.frame(sim.det.out[v+90]))
    }else {if(length(unitNumList)==20){
      rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+5]),as.data.frame(sim.det.out[v+10]),as.data.frame(sim.det.out[v+15]),as.data.frame(sim.det.out[v+20]) ,
            as.data.frame(sim.det.out[v+25]) ,as.data.frame(sim.det.out[v+30]) ,as.data.frame(sim.det.out[v+35]),as.data.frame(sim.det.out[v+40]) ,as.data.frame(sim.det.out[v+45]),as.data.frame(sim.det.out[v+50]),
            as.data.frame(sim.det.out[v+55]),as.data.frame(sim.det.out[v+60]),as.data.frame(sim.det.out[v+65]),as.data.frame(sim.det.out[v+70]),as.data.frame(sim.det.out[v+75]),as.data.frame(sim.det.out[v+80]),as.data.frame(sim.det.out[v+85]),as.data.frame(sim.det.out[v+90]),as.data.frame(sim.det.out[v+95]))
    }else {if(length(unitNumList)==21){
      rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+5]),as.data.frame(sim.det.out[v+10]),as.data.frame(sim.det.out[v+15]),as.data.frame(sim.det.out[v+20]) ,
            as.data.frame(sim.det.out[v+25]) ,as.data.frame(sim.det.out[v+30]) ,as.data.frame(sim.det.out[v+35]),as.data.frame(sim.det.out[v+40]) ,as.data.frame(sim.det.out[v+45]),as.data.frame(sim.det.out[v+50]),
            as.data.frame(sim.det.out[v+55]),as.data.frame(sim.det.out[v+60]),as.data.frame(sim.det.out[v+65]),as.data.frame(sim.det.out[v+70]),as.data.frame(sim.det.out[v+75]),as.data.frame(sim.det.out[v+80]),as.data.frame(sim.det.out[v+85]),as.data.frame(sim.det.out[v+90]),as.data.frame(sim.det.out[v+95]),as.data.frame(sim.det.out[v+100]))
    }else {if(length(unitNumList)==22){
      rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+5]),as.data.frame(sim.det.out[v+10]),as.data.frame(sim.det.out[v+15]),as.data.frame(sim.det.out[v+20]) ,
            as.data.frame(sim.det.out[v+25]) ,as.data.frame(sim.det.out[v+30]) ,as.data.frame(sim.det.out[v+35]),as.data.frame(sim.det.out[v+40]) ,as.data.frame(sim.det.out[v+45]),as.data.frame(sim.det.out[v+50]),
            as.data.frame(sim.det.out[v+55]),as.data.frame(sim.det.out[v+60]),as.data.frame(sim.det.out[v+65]),as.data.frame(sim.det.out[v+70]),as.data.frame(sim.det.out[v+75]),as.data.frame(sim.det.out[v+80]),as.data.frame(sim.det.out[v+85]),as.data.frame(sim.det.out[v+90]),as.data.frame(sim.det.out[v+95]),as.data.frame(sim.det.out[v+100]),as.data.frame(sim.det.out[v+105]))
    }else {if(length(unitNumList)==23){
      rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+5]),as.data.frame(sim.det.out[v+10]),as.data.frame(sim.det.out[v+15]),as.data.frame(sim.det.out[v+20]) ,
            as.data.frame(sim.det.out[v+25]) ,as.data.frame(sim.det.out[v+30]) ,as.data.frame(sim.det.out[v+35]),as.data.frame(sim.det.out[v+40]) ,as.data.frame(sim.det.out[v+45]),as.data.frame(sim.det.out[v+50]),
            as.data.frame(sim.det.out[v+55]),as.data.frame(sim.det.out[v+60]),as.data.frame(sim.det.out[v+65]),as.data.frame(sim.det.out[v+70]),as.data.frame(sim.det.out[v+75]),as.data.frame(sim.det.out[v+80]),as.data.frame(sim.det.out[v+85]),as.data.frame(sim.det.out[v+90]),as.data.frame(sim.det.out[v+95]),
            as.data.frame(sim.det.out[v+100]),as.data.frame(sim.det.out[v+105]),as.data.frame(sim.det.out[v+110]))
    }else {if(length(unitNumList)==24){
      rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+5]),as.data.frame(sim.det.out[v+10]),as.data.frame(sim.det.out[v+15]),as.data.frame(sim.det.out[v+20]) ,
            as.data.frame(sim.det.out[v+25]) ,as.data.frame(sim.det.out[v+30]) ,as.data.frame(sim.det.out[v+35]),as.data.frame(sim.det.out[v+40]) ,as.data.frame(sim.det.out[v+45]),as.data.frame(sim.det.out[v+50]),
            as.data.frame(sim.det.out[v+55]),as.data.frame(sim.det.out[v+60]),as.data.frame(sim.det.out[v+65]),as.data.frame(sim.det.out[v+70]),as.data.frame(sim.det.out[v+75]),as.data.frame(sim.det.out[v+80]),as.data.frame(sim.det.out[v+85]),as.data.frame(sim.det.out[v+90]),as.data.frame(sim.det.out[v+95]),
            as.data.frame(sim.det.out[v+100]),as.data.frame(sim.det.out[v+105]),as.data.frame(sim.det.out[v+110]),as.data.frame(sim.det.out[v+115]))
    }else {if(length(unitNumList)==25){
      rbind(as.data.frame(sim.det.out[v]),as.data.frame(sim.det.out[v+5]),as.data.frame(sim.det.out[v+10]),as.data.frame(sim.det.out[v+15]),as.data.frame(sim.det.out[v+20]) ,
            as.data.frame(sim.det.out[v+25]) ,as.data.frame(sim.det.out[v+30]) ,as.data.frame(sim.det.out[v+35]),as.data.frame(sim.det.out[v+40]) ,as.data.frame(sim.det.out[v+45]),as.data.frame(sim.det.out[v+50]),
            as.data.frame(sim.det.out[v+55]),as.data.frame(sim.det.out[v+60]),as.data.frame(sim.det.out[v+65]),as.data.frame(sim.det.out[v+70]),as.data.frame(sim.det.out[v+75]),as.data.frame(sim.det.out[v+80]),as.data.frame(sim.det.out[v+85]),as.data.frame(sim.det.out[v+90]),as.data.frame(sim.det.out[v+95]),
            as.data.frame(sim.det.out[v+100]),as.data.frame(sim.det.out[v+105]),as.data.frame(sim.det.out[v+110]),as.data.frame(sim.det.out[v+115]),as.data.frame(sim.det.out[v+120]))
    }}}}}}}}}}}}}}}}}}}}}}}}}

    new.sim.det.out[[v]]<-new.out

  }

  cell.2=sort(seq(1:npoints))

  return(list(detList=new.sim.det.out, covList=sim.Covs.out, N=sim.N.out, cell=cell.2))

}#end function
