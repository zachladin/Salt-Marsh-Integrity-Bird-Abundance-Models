#resultsDirectory<-"/Users/Zach/Dropbox/ZachGreg/Bayes_PowerAnalysis/Full_Model_7-22-16/Results/Abundance_Estimates/TMO"
#eventually (with all SHARP data included), directory<-paste(getwd(),"Results", "Full", "Abundance_Unit", "TMO",sep="/)

#function to get Core units (i.e., units with highest abundance estimate from each refuge)
getCoreUnits<-function(dataIn, speciesName,resultsDirectory){

  #point at TMO results from Abundance_Unit to get "core" units
  setwd(resultsDirectory)

  #read in full data set
  new.data<-data

  #get refugeList
  refugeList<-sort(unique(as.character(new.data$NWR_code)))

  #compile all results into one file for species
  core.units.save<-list()
  for(i in 1:length(refugeList)){

    new.results<-
      tryCatch({
        cond1=read.csv(paste(getwd(),refugeList[i],paste(speciesName,".abundance.by.year.and.unit.csv",sep=""),sep="/"),header=TRUE)
        cond1
      },error=function(cond2){
          cond2=NA
          cond2
      })

    if(is.na(new.results)==FALSE){

      get.unit<-function(){
        #get mean by unit
        try(unit.means<-summaryFunction(dataIn=new.results, response="Predicted",factor="SMI_Unit"))

        #sort by mean
        try(units.rank<-unit.means[order(unit.means$mean,decreasing = TRUE),])

        #identify core unit
        try(core.unit<-data.frame(NWR_code=refugeList[i], Core_unit=as.character(units.rank[1,1])))

        return(core.unit)
        }
      core.unit<-get.unit()
      } else{
        core.unit<-data.frame(AlphaCode=speciesName,NWR_code=refugeList[i], Core_unit=NA)
        }

    #compile data.frame of core units from all refuges
    core.units.save<-rbind(core.units.save, core.unit)
  }

  #save list of core units to 'Data'
  write.csv(core.units.save, file=paste(getwd(),"Data",paste(speciesName,".core.units.list.csv",sep=""),sep="/"),row.names=FALSE)

return(core.units.save)
}
#End