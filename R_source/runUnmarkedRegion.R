#use 'unmarked' to run frequentist hierarchical linear models using gmultmix function
runUnmarkedRegion<-function(dataIn, speciesName){

  new.data<-dataIn

  #make umf
  umf<-makeGMMumf(dataIn=new.data,species=speciesName)

  #run model in unmarked
  mod.1<-tryCatch({
    message("Running model with mixture=NB.")
    cond1=gmultmix(~Year+NWR_code, ~1, ~VisitNum, data=umf,mixture="NB")
    cond1
    },error=function(cond2){
      message("Running model with mixture=P.")
      cond2=gmultmix(~Year+NWR_code, ~1, ~VisitNum, data=umf,mixture="P")
      cond2
      },error=function(cond3){
        message("Running model without 'VisitNum' as det. covariate and mixture=NB.")
        cond3=gmultmix(~Year+NWR_code, ~1, ~1, data=umf,mixture="NB")
        cond3
        },error=function(cond4){
          message("Running model withougt 'VisitNum' as det. covariate and mixture=P.")
          cond4=gmultmix(~Year+NWR_code, ~1, ~1, data=umf,mixture="P")
          cond4
        }
  )

  return(mod.1)
}

#End