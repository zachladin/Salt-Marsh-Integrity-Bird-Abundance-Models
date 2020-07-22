#runUnmarkedAllYears function
runUnmarkedAllYearsRegionAllSHARP<-function(dataIn, speciesName){

  new.data<-dataIn

  #make umf
  umf<-makeGMMumfRegion(dataIn=new.data,species=speciesName)

  #run model in unmarked
  mod.1<-tryCatch({
    message(paste("Running model with mixture=NB for ",speciesName,".",sep=""))
    cond1=gmultmix(~1, ~1, ~VisitNum, data=umf,mixture="NB")
    cond1
    },error=function(cond2){
      message(paste("Running model with mixture=P for ",speciesName,".",sep=""))
      cond2=gmultmix(~1, ~1, ~VisitNum, data=umf,mixture="P")
      cond2
    }
  )


  return(mod.1)
}

#End