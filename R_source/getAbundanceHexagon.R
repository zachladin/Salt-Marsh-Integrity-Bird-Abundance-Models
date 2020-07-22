#function to get abundance estimates
getAbundanceHexagon<-function(dataIn, speciesIn){

  hex.data<-dataIn

  speciesName=speciesIn
  
  #run runUnmarked function
  mod.1<-tryCatch({runUnmarkedAllYearsHexagon(dataIn=hex.data, speciesIn=speciesName)},
                  error=function(cond1){
                    cond1=NULL
                    message("Data did not meet minimal requirements for successful model fitting.")
                  })
  #run getModelOutput function
  message("Saving model results")
  abun.out<-getModelOutputAllYearsHexagon(dataIn=hex.data, modelIn=mod.1, speciesIn=speciesName)


  #run runUnmarked function
  mod.2<-tryCatch({runUnmarkedHexagon(dataIn=hex.data, speciesIn=speciesName)},
                  error=function(cond1){
                    cond1=NULL
                    message("Data did not meet minimal requirements for successful model fitting.")
                  })
  #run getModelOutput function
  message("Saving model results")
  getModelOutputHexagon(dataIn=hex.data, modelIn=mod.2, speciesIn=speciesName)

  return(abun.out)
}

#End