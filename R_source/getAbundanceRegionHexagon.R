#function to get abundance estimates by Region
getAbundanceRegionHexagon<-function(dataIn, speciesName){

  region.data<-dataIn

  #run runUnmarked function
  mod.1<-tryCatch({runUnmarkedAllYearsRegionHexagon(dataIn=region.data, speciesName=speciesName)},
                  error=function(cond1){
                    cond1=NULL
                    message("Data did not meet minimal requirements for successful model fitting.")
                  })
  #run getModelOutput function
  message("Saving model results")
  getModelOutputAllYearsRegionHexagon(dataIn=region.data, modelIn=mod.1, speciesName=speciesName)

  #run runUnmarked function
  mod.2<-tryCatch({runUnmarkedRegionHexagon(dataIn=region.data, speciesName=speciesName)},
                  error=function(cond1){
                    cond1=NULL
                    message("Data did not meet minimal requirements for successful model fitting.")
                  })
  #run getModelOutput function
  message("Saving model results")
  getModelOutputRegionHexagon(dataIn=region.data, modelIn=mod.2, speciesName=speciesName)

}

#End