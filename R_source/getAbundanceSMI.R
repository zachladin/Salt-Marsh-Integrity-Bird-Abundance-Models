#function to get abundance estimates
getAbundanceSMI<-function(dataIn, speciesName){

  refuge.data<-dataIn

  #run runUnmarked function
  mod.all<-tryCatch({runUnmarkedAllYears(dataIn=refuge.data, speciesName=speciesName)},
                  error=function(cond1){
                    cond1=NULL
                    message("Data did not meet minimal requirements for successful model fitting.")
                  })
  #run getModelOutput function
  message("Saving model results")

  output<-tryCatch({getModelOutputAllYears(dataIn=refuge.data, modelIn=mod.all, speciesName=speciesName)},
           error=function(cond1){
             cond1=NULL
           })

  return(output)
}

#End