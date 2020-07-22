#use 'unmarked' to run frequentist hierarchical linear models using gmultmix function
runUnmarkedNWR<-function(dataIn, speciesName){
  
  new.data<-dataIn
  
  #make umf
  umf<-makeGMMumf(dataIn=new.data,species=speciesName)
  
  #run model in unmarked
  mod.1<-tryCatch({
    message("Running model with mixture=NB.")
    cond1=gmultmix(~Year, ~1, ~VisitNum, data=umf,mixture="NB")
    cond1
    },error=function(cond2){
      message("Running model with mixture=P.")
      cond2=gmultmix(~Year, ~1, ~VisitNum, data=umf,mixture="P")
      cond2
        }
  )
  
  return(mod.1)
}

#End