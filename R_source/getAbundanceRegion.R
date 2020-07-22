#function to get abundance estimates by Region
getAbundanceRegion<-function(dataIn, speciesName){

  region.data<-dataIn

  #run runUnmarked function
  mod.all<-tryCatch({runUnmarkedAllYearsRegion(dataIn=region.data, speciesName=speciesName)},
                  error=function(cond1){
                    cond1=NULL
                    message("Data did not meet minimal requirements for successful model fitting (All years).")
                  })
  #run getModelOutput function
  message("Saving model results")
  getModelOutputAllYearsRegion(dataIn=region.data, modIn=mod.all, speciesName=speciesName)

  # #run runUnmarked function
  # mod.1<<-tryCatch({runUnmarkedRegion(dataIn=region.data, speciesName=speciesName)},
  #                 error=function(cond1){
  #                 cond1=NULL
  #                  message("Data did not meet minimal requirements for successful model fitting (by each year).")
  #               })
  # #run getModelOutput function
  # getModelOutputRegion(dataIn=region.data, modIn=mod.1, speciesName=speciesName)
  
return(mod.all)
}

#End