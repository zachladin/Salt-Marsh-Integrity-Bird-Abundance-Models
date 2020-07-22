getRefugeData<-function(dataIn){

  data<-dataIn

  #make folder for refuge data
  dir.create(paste(getwd(),"Data","Refuge_data",sep="/"))
  #############################
  #refugeData function

  refugeData<-function(dataIn,refugeCode){
    data<-dataIn
    ref.data<-subset(data, NWR_code==refugeCode)
    ref.data$PointID<-as.factor(as.character(ref.data$PointID))
    ref.data$SMI_Unit<-as.factor(as.character(ref.data$SMI_Unit))
    ref.data$UnitNum<-as.factor(as.character(ref.data$UnitNum))
    ref.data$Year.Point.Visit<-as.factor(as.character(ref.data$Year.Point.Visit))
    ref.data$Year<-as.factor(as.character(ref.data$Year))
    return(ref.data)
  }
  #############################
  data.tmo<-data

  #replace focal spp. with "TMO"
  data.tmo$AlphaCode <- as.character(data.tmo$AlphaCode)
  data.tmo$AlphaCode[data.tmo$AlphaCode == "SALS"] <- "TMO"
  data.tmo$AlphaCode[data.tmo$AlphaCode == "SESP"] <- "TMO"
  data.tmo$AlphaCode[data.tmo$AlphaCode == "NESP"] <- "TMO"
  data.tmo$AlphaCode[data.tmo$AlphaCode == "CLRA"] <- "TMO"
  data.tmo$AlphaCode[data.tmo$AlphaCode == "WILL"] <- "TMO"


  data.tmo$AlphaCode<-as.factor(as.character(data.tmo$AlphaCode))
  #########################################################################################
  #subset and save Refuge data for all focal species

  #get speciesList
  speciesList<-sort(c("TMO","WILL","SALS","SESP","NESP","CLRA"))

  #get list of refuges
  refugeList<-sort(as.character(unique(data$NWR_code)))

  #for loop to create and save data by refuge all species
  message("Subsetting and saving data by NWR.")
  for(i in 1:length(refugeList)){
    message(refugeList[i])
    new.data<-refugeData(dataIn=data, refugeCode=refugeList[i])

    refugeName<-as.character(unique(new.data$NWR_code))

    dir.create(paste(getwd(),"Data","Refuge_data",refugeName,sep="/"))

    write.csv(new.data, paste(getwd(),"Data","Refuge_data",refugeName,paste(refugeName,"data.csv",sep="."),sep="/"),row.names=FALSE)

  }

  #for loop to create and save data by refuge TMO
  message("Subsetting and saving data by NWR for TMO.")
  for(i in 1:length(refugeList)){
    message(paste(refugeList[i]," TMO",sep=""))
    new.data<-refugeData(dataIn=data.tmo, refugeCode=refugeList[i])

    refugeName<-as.character(unique(new.data$NWR_code))

    write.csv(new.data, paste(getwd(),"Data","Refuge_data",refugeName,paste(refugeName,"data.tmo.csv",sep="."),sep="/"),row.names=FALSE)

  }



}
#End
