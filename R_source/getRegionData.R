getRegionData<-function(dataIn){

  data<-dataIn

  #############################
  #regionData function

  regionData<-function(dataIn,regionName){
    data<-dataIn
    ref.data<-subset(data, RegionName==regionName)
    ref.data$PointID<-as.factor(as.character(ref.data$PointID))
    ref.data$SMI_Unit<-as.factor(as.character(ref.data$SMI_Unit))
    ref.data$UnitNum<-as.factor(as.character(ref.data$UnitNum))
    ref.data$Year.Point.Visit<-as.factor(as.character(ref.data$Year.Point.Visit))
    ref.data$Year<-as.factor(as.character(ref.data$Year))
    return(ref.data)
  }
  #############################

  #make TMO column
  data$AlphaCode<-as.character(data$AlphaCode)
  data.sub<-subset(data, c(AlphaCode=="SALS" | AlphaCode=="SESP" | AlphaCode=="WILL" | AlphaCode=="NESP" | AlphaCode=="CLRA"))
  species<-sort(unique(data.sub$AlphaCode))
  data.sub$AlphaCode<-as.factor(data.sub$AlphaCode)

  data.tmo<-data.sub
  data.tmo$AlphaCode<-"TMO"

  #########################################################################################
  #create some folders for Results
  dataFilepath<-paste(getwd(),"Data",sep="/")
  regionDataFilepath<-paste(getwd(),"Data","Data_region",sep="/")

  dir.create(path=regionDataFilepath)

  #create subfolders for focal species (SALS, SESP, NESP, CLRA, WILL)
  speciesList<-sort(c("TMO","WILL","SALS","SESP","NESP","CLRA"))

  #get list of regions
  regionList<-sort(unique(data$RegionName))

  ########################################
  #subset and save region data for all focal species

  #for loop to create and save data by region all species
  message("Subsetting and saving data by Region.")
  for(i in 1:length(regionList)){
    message(regionList[i])
    new.data<-regionData(dataIn=data, regionName=regionList[i])

    write.csv(new.data, paste(regionDataFilepath,paste(regionList[i],"data.csv",sep="."),sep="/"),row.names=FALSE)

  }

  #for loop to create and save data by region TMO
  message("Subsetting and saving data by Region for TMO.")
  for(i in 1:length(regionList)){
    message(paste(regionList[i]," TMO",sep=""))

    new.data<-regionData(dataIn=data.tmo, regionName=regionList[i])

    write.csv(new.data, paste(regionDataFilepath,paste(regionList[i],"data.tmo.csv",sep="."),sep="/"),row.names=FALSE)

  }



}
#End
