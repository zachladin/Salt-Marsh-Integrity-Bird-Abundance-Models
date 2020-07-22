#runUnmarkedAllYears function
runUnmarkedAllYearsStateHexagon<-function(dataIn, speciesName){

  new.data<-dataIn

  #make umf
  umf<-makeGMMumf(dataIn=new.data,species=speciesName)

  #run model in unmarked
  mod.1<-tryCatch({
    message("Running model for all years with mixture=NB.")
    cond1=gmultmix(~Hexagon, ~1, ~VisitNum, data=umf,mixture="NB")
    cond1
    },error=function(cond2){
      message("Running model for all years with mixture=P.")
      cond2=gmultmix(~Hexagon, ~1, ~VisitNum, data=umf,mixture="P")
      cond2
    },error=function(cond3){
      message("Running modelfor all years without 'VisitNum' as detection covariate and mixture=NB.")
      cond3=gmultmix(~Hexagon, ~1, ~1, data=umf,mixture="NB")
      cond3
      },error=function(cond4){
        message("Running model for all years without 'VisitNum' as detection covariate and mixture=P.")
        cond4=gmultmix(~Hexagon, ~1, ~1, data=umf,mixture="P")
        cond4
      }
  )


  return(mod.1)
}

#End