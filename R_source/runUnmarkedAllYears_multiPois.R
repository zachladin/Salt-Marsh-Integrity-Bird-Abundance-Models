#runUnmarkedAllYears function
runUnmarkedAllYears_multiPois<-function(dataIn, speciesName){

  new.data<-dataIn

  #make umf
  umf<-makeMultPoisumf(dataIn=new.data,species=speciesName)

  #try with multPois
  mod.1<-tryCatch({
    message("Running model with mixture=NB and Visit and Observer.")
    cond1=multinomPois(~VisitNum+Observer ~UnitNum , data=umf)
    cond1
  },error=function(cond2){
    message("Running model with mixture=P and Visit and Observer.")
    cond2=multinomPois(~VisitNum+Observer ~UnitNum , data=umf)
    cond2
  },error=function(cond3){
    message("Running model with mixture=NB with just Visit.")
    cond3=multinomPois(~VisitNum ~UnitNum, data=umf)
    cond3
  })
  


  return(mod.1)
}

#End