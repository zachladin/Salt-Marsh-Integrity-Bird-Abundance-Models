#function to get abundance estimates
getAbundance<-function(dataIn, speciesName,filePath){

  refuge.data<-dataIn

  #run runUnmarked function
  mod.all<-tryCatch({runUnmarkedAllYears(dataIn=refuge.data, speciesName=speciesName)},
                  error=function(cond1){
                    cond1=NULL
                    message("Data did not meet minimal requirements for successful model fitting.")
                  })
  #run getModelOutput function
  message("Saving model results")
  try(getModelOutputAllYears(dataIn=refuge.data, modelIn=mod.all, speciesName=speciesName,filePath=filePath))


  # #run runUnmarked function
  mod.2<-tryCatch({runUnmarked(dataIn=refuge.data, speciesName=speciesName)},
                  error=function(cond1){
                    cond1=NULL
                    message("Data did not meet minimal requirements for successful model fitting.")
                  })
  #run getModelOutput function
  message("Saving model results")
  try(getModelOutput(dataIn=refuge.data, modelIn=mod.2, speciesName=speciesName,filePath=filePath))

  return(mod.all)
}

#End