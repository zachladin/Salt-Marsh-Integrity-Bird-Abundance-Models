getHexagonData<-function(dataIn){

  data<-dataIn

  #############################
  #refugeData function

  hexagonData<-function(dataIn,hexCode){
    data<-dataIn
    hex.data<-subset(data, Hexagon==hexCode)
    hex.data$PointID<-as.factor(as.character(hex.data$PointID))
    hex.data$SMI_Unit<-as.factor(as.character(hex.data$SMI_Unit))
    hex.data$UnitNum<-as.factor(as.character(hex.data$UnitNum))
    hex.data$Hexagon<-as.factor(as.character(hex.data$Hexagon))
    hex.data$Year.Point.Visit<-as.factor(as.character(hex.data$Year.Point.Visit))
    hex.data$Year<-as.factor(as.character(hex.data$Year))
    return(hex.data)
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
  hexagonDataFilepath<-paste(getwd(),"Data","Data_hexagon",sep="/")

  dir.create(path=hexagonDataFilepath)
  dir.create(path=resultsFilepath)

  ########################################
  #subset and save Refuge data for all focal species

  #get speciesList
  speciesList<-sort(c("TMO","WILL","SALS","SESP","NESP","CLRA"))

  #get list of refuges
  hexList<-sort(unique(data$Hexagon))

  #for loop to create and save data by refuge all species
  message("Subsetting and saving data by Hexagon.")
  for(i in 1:length(hexList)){
    message(hexList[i])
    new.data<-hexagonData(dataIn=data, hexCode=hexList[i])

    write.csv(new.data, paste(hexagonDataFilepath,paste(hexList[i],"data.csv",sep="."),sep="/"),row.names=FALSE)

  }

  #for loop to create and save data by refuge TMO
  message("Subsetting and saving data by Hexagon for TMO.")
  for(i in 1:length(hexList)){
    message(paste(hexList[i]," TMO",sep=""))
    new.data<-hexagonData(dataIn=data.tmo, hexCode=hexList[i])

    write.csv(new.data, paste(hexagonDataFilepath,paste(hexList[i],"data.tmo.csv",sep="."),sep="/"),row.names=FALSE)

  }



}
#End
