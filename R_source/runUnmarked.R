#use 'unmarked' to run frequentist hierarchical linear models using gmultmix function
runUnmarked<-function(dataIn, speciesName){

  new.data<-dataIn

  #make umf
  umf<-makeGMMumf(dataIn=new.data,species=speciesName)

  #run model in unmarked
  mod.1<-tryCatch({
    message("Running model with mixture=NB and Visit.")
    cond1=gmultmix(~Year+UnitNum, ~1, ~VisitNum, data=umf,mixture="NB")
    cond1
    },warning=function(cond2){
      message("Running model with mixture=P and Visit.")
      cond2=gmultmix(~Year+UnitNum, ~1, ~VisitNum, data=umf,mixture="P")
      cond2
    }
  )

  return(mod.1)
}

#End