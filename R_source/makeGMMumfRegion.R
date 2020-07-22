#make Generalized Multinomial Model unmarkedFrame
makeGMMumfRegion<-function(dataIn,species){

  data.new<-dataIn

  dataRegion<-unique(data.new$RegionNum)
  regionName<-sort(unique(as.character(data.new$RegionName)))

  data.new$PointID<-as.factor(as.character(data.new$PointID))

  #data.new$VisitNum<-as.character(data.new$VisitNum)

  #subset data to remove visits 0 and 4 from data
  data.new<-subset(data.new, c(VisitNum != 4 & VisitNum != 0))

  data.new$Year.Point.Visit<-as.factor(as.character(data.new$Year.Point.Visit))

  #get detection data and covs with melt and cast
  data.count.melt <- melt(data.new, id=c("PointID","AlphaCode","DistCode","VisitNum","Year","Year.Unit","Year.Point","Year.Point.Visit","State","RegionNum","NWR_code","SMI_Unit","UnitNum","Hexagon"),
                          measure=c("Min.1","Min.2","Min.3","Min.4","Min.5"), na.rm=FALSE)

  #cast count data (one visit at first) add try()
  count.data <- cast(data.count.melt, Year.Point.Visit ~ variable, add.missing=TRUE,fun.aggregate=sum, subset=c(AlphaCode==species & DistCode==1))
  head(count.data)

  #get nYears, nSites
  site.covs<-read.table(text=as.character(count.data$Year.Point),colClasses="character",sep=".")
  count.data.out.1<-cbind(site.covs,count.data)
  colnames(count.data.out.1)[1:3]<-c("Year","PointID","VisitNum")
  count.data.out.1$PointID<-as.factor(as.character(count.data.out.1$PointID))
  count.data.out.1$Year<-as.factor(as.character(count.data.out.1$Year))

  #get some site covariates from new.data
  head(data.new)
  data.new.covs<-data.new[,c("Year.Point.Visit","State","RegionNum","NWR_code","SMI_Unit","UnitNum","Hexagon",
                             "Observer","Month","Day","time.scale","temp.scale","Tide")]

  data.new.covs.unique<-unique(data.new.covs)

  data.out.1<-merge(count.data.out.1, data.new.covs.unique, by="Year.Point.Visit",all=FALSE)
  count.data.out<-unique(data.out.1)

  #number of survey locations
  nSites<-length(unique(count.data.out$PointID))

  #number of visits
  nVisits<-3

  #number of intervals
  nIntervals<-5

  #save data in long format to working directory
  write.csv(count.data.out,paste(speciesUMFdataFilepath,paste(species,regionName,"100_long.csv",sep="."),sep="/"),row.names=FALSE)

  #########################################################################################
  # Import data and check structure
  species.data <- read.csv(paste(speciesUMFdataFilepath,paste(species,regionName,"100_long.csv",sep="."),sep="/"), na.strings = c("-Inf", "NA","<NA>"),check.names=FALSE)
  species.data$VisitNum<-as.factor(as.character(species.data$VisitNum))
  species.data$Year<-as.factor(as.character(species.data$Year))
  head(species.data)

  #########################################################################################

  y<-species.data[,c("Min.1","Min.2","Min.3", "Min.4", "Min.5")]
  covs<-species.data[,c("Year","State","RegionNum","NWR_code","SMI_Unit","UnitNum","Hexagon","VisitNum","Month","Day",
                        "Observer","time.scale","temp.scale","Tide")]
  covs$UnitNum<-as.factor(as.character(covs$UnitNum))
  covs$Hexagon<-as.factor(as.character(covs$Hexagon))
  obs.covs<-species.data[,c("Month","Day","Observer","time.scale","temp.scale","Tide")]
  head(covs)


  #now bundle everything into unmarkedFrame
  umf <- unmarkedFrameGMM(y=y,

                          siteCovs=covs,
                          numPrimary=1,type = "removal")

  return(umf)
}

#End