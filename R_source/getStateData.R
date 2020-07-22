getStateData<-function(dataIn){

  data<-dataIn

  #############################
  #regionData function

  stateData<-function(dataIn,stateName){
    data<-dataIn
    ref.data<-subset(data, State==stateName)
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
  stateDataFilepath<-paste(getwd(),"Data","Data_state",sep="/")

  dir.create(path=stateDataFilepath)

  #create subfolders for focal species (SALS, SESP, NESP, CLRA, WILL)
  speciesList<-sort(c("TMO","WILL","SALS","SESP","NESP","CLRA"))

  #get list of regions
  stateList<-sort(unique(data$State))

  ########################################
  #subset and save region data for all focal species

  #for loop to create and save data by region all species
  message("Subsetting and saving data by State.")
  for(i in 1:length(stateList)){
    message(stateList[i])
    new.data<-stateData(dataIn=data, stateName=stateList[i])

    write.csv(new.data, paste(stateDataFilepath,paste(stateList[i],"data.csv",sep="."),sep="/"),row.names=FALSE)

  }

  #for loop to create and save data by region TMO
  message("Subsetting and saving data by State for TMO.")
  for(i in 1:length(stateList)){
    message(paste(stateList[i]," TMO",sep=""))
    new.data<-stateData(dataIn=data.tmo, stateName=stateList[i])

    write.csv(new.data, paste(stateDataFilepath,paste(stateList[i],"data.tmo.csv",sep="."),sep="/"),row.names=FALSE)

  }



}
#End
