#function to get abundance estimates by Region
getAbundanceNWR<-function(dataIn, speciesName){

  NWR.data<-dataIn

  #run runUnmarked function
  mod.1<-tryCatch({runUnmarkedAllYearsNWR(dataIn=NWR.data, speciesName=speciesName)},
                  error=function(cond1){
                    cond1=NULL
                    message("Data did not meet minimal requirements for successful model fitting.")
                  })
  #run getModelOutput function
  message("Saving model results")
  getModelOutputAllYearsNWR(dataIn=NWR.data, modelIn=mod.1, speciesName=speciesName)

  #run runUnmarked function
  mod.2<-tryCatch({runUnmarkedNWR(dataIn=NWR.data, speciesName=speciesName)},
                  error=function(cond1){
                    cond1=NULL
                    message("Data did not meet minimal requirements for successful model fitting.")
                  })
  #run getModelOutput function
  message("Saving model results")
  getModelOutputNWR(dataIn=NWR.data, modelIn=mod.2, speciesName=speciesName)

}

#End