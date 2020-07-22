#plot abundance estiates
getModelOutputTableRegionSHARP<-function(dataIn, modelIn, speciesName){

  new.data<-dataIn
  regionName<-unique(new.data$RegionName)

  dir.create(paste(getwd(), "Results","SHARP_Region",sep="/"))
  dir.create(paste(getwd(), "Results","SHARP_Region",regionName,sep="/"))
  regionFilepath_full<-paste(getwd(),"Results","SHARP_Region",regionName,speciesName,sep="/")
  dir.create(regionFilepath_full)

   mod<-modelIn

  mod.1.out<-predict(mod,type="lambda",appendData=TRUE)
  mod.1.out$RegionName<-regionName

  #add species
  mod.1.out$Species<-speciesName

  #get columns to create table of Abundance estimates
  species.abun<-unique(mod.1.out[, c("Species","Year","RegionName","Predicted","SE","lower","upper")])

  #add column with model formula
  model.formula<-paste(deparse(mod@call)[1], deparse(mod@call)[2],sep="")
  species.abun$model.formula<-model.formula

  #reorder table of results
  species.abun.table<-species.abun[order(species.abun$RegionName),]

  #save table as .csv file
  write.csv(species.abun.table, file=paste(regionFilepath_full,paste(speciesName,"abundance.by.region.years.csv",sep="."),sep="/"),row.names=FALSE)

  return(species.abun.table)

}

#End