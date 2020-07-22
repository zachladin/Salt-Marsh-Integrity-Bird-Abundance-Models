#runUnmarkedAllYearsHexagon function
runUnmarkedAllYearsHexagon<-function(dataIn, speciesIn){

  new.data<-dataIn

  speciesName=speciesIn
  
  #make umf
  umf<-makeGMMumf(dataIn=new.data,speciesIn=speciesName)

  #run model in unmarked
  mod.1<-tryCatch({
    message("Running model with mixture=NB.")
    cond1=gmultmix(~1, ~1, ~VisitNum, data=umf,mixture="NB")
    cond1
    },warning=function(cond2){
      message("Running model with mixture=P.")
      cond2=gmultmix(~1, ~1, ~VisitNum, data=umf,mixture="P")
      cond2
    },warning=function(cond3){
      message("Running model without 'VisitNum' as detection covariate and mixture=NB.")
      cond3=gmultmix(~1, ~1, ~1, data=umf,mixture="NB")
      cond3
      },warning=function(cond4){
        message("Running model without 'VisitNum' as detection covariate and mixture=P.")
        cond4=gmultmix(~1, ~1, ~1, data=umf,mixture="P")
        cond4
      }
  )


  return(mod.1)
}

#End