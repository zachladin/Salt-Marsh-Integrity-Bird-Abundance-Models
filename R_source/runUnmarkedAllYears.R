#runUnmarkedAllYears function
runUnmarkedAllYears<-function(dataIn, speciesName){

  new.data<-dataIn

  #make umf
  umf<-makeGMMumf(dataIn=new.data,species=speciesName)

  #run model in unmarked
  mod.1<-tryCatch({
    message("Running model with mixture=NB and Visit and Observer.")
    cond1=gmultmix(~UnitNum, ~1, ~VisitNum+Observer, data=umf,mixture="NB")
    cond1
    },error=function(cond2){
      message("Running model with mixture=P and Visit and Observer.")
      cond2=gmultmix(~UnitNum, ~1, ~VisitNum+Observer, data=umf,mixture="P")
      cond2
    },error=function(cond3){
      message("Running model with mixture=NB with just Visit.")
      cond3=gmultmix(~UnitNum, ~1, ~VisitNum, data=umf,mixture="NB")
      cond3
    })


  return(mod.1)
}

#End