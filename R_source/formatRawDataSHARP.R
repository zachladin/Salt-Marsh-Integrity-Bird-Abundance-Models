#format raw data and save .csv file
formatRawDataSHARP<-function(dataIn,saveFileName){

#read in raw data and format
raw.data<-dataIn
new.data<-raw.data

#add Month, Year, and Day columns to data
message("Formatting SHARP data.")
date.data<-new.data$SurveyDate
date.data<-as.character(date.data)
date.string<-read.table(text=as.character(date.data), sep="/",colClasses = "character")
colnames(date.string)<-c("Month","Day","Year")
date.string$Year<-as.factor(paste("20",date.string$Year,sep=""))

date.string$new.date<-as.character(paste(date.string$Year,date.string$Month,date.string$Day,sep="-"))
date.string$Day <- as.integer(format(as.Date(date.string$new.date, format = "%Y-%m-%d"), "%j"))

new.data.1<-cbind(new.data,date.string)
head(new.data.1)

stateList<-sort(unique(as.character(new.data.1$State)))
new.data.1$State<-as.character(new.data.1$State)

#data with errors (3 rows)
message("Resolving errors found in data.")

#read in manager data to merge with data
covs<-read.csv(paste(getwd(), "Data", "Region_Refuge_code_lookup.csv", sep="/"))
names(covs)[names(covs)=="KIRA"]<-"KIRA_sampled"
names(covs)[names(covs)=="region_ID"]<-"PointID"
head(covs)

#merge data with covs
data.merge<-merge(new.data.1, covs, by=c("PointID"))
head(data.merge)

#remove desired columns
data.rem<-data.merge[ , !(names(data.merge) %in% c("RegionNum.y","State.y","Hexagon.y"))]
head(data.rem)

#rename columns
names(data.rem)[names(data.rem)=="RegionNum.x"]<-"RegionNum"
names(data.rem)[names(data.rem)=="State.x"]<-"State"
names(data.rem)[names(data.rem)=="Hexagon.x"]<-"Hexagon"

#create unit_ID numeric identifier (only have to do this one time)
new.data.2<-transform(data.rem, UnitNum=as.numeric(factor(SMI_Unit)))

#convert Start_Time to decimal (numeric)
timeConvertDecimal<-function(DataIn){
  time.data<-DataIn
  time.data<-as.character(time.data$SurveyTime)
  time.data<-substr(time.data,1,4)

  time.new<-round(sapply(strsplit(time.data,":"),
                         function(x) {
                           x <- as.numeric(x)
                           x[1]+x[2]/60
                         }),3)
  return(time.new)
}

new.data.2$time2<-timeConvertDecimal(DataIn=new.data.2)
new.data.2$time2 <- as.numeric(as.character(new.data.2$time2))

#create DistCode (combine 0-50m and 51-100m distance bands)
new.data.2$DistCode<-ifelse(new.data.2$DistBand=="0-50m",1,ifelse(new.data.2$DistBand=="51-100m",1,0))

#subset data to only include 0-100m
message("Subsetting data to include all detections 0-100m")
new.data.3<-subset(new.data.2, DistCode==1)

#convert count data to integers
new.data.3$Min.1<-as.integer(as.character(new.data.3$Min.1))
new.data.3$Min.2<-as.integer(as.character(new.data.3$Min.2))
new.data.3$Min.3<-as.integer(as.character(new.data.3$Min.3))
new.data.3$Min.4<-as.integer(as.character(new.data.3$Min.4))
new.data.3$Min.5<-as.integer(as.character(new.data.3$Min.5))

#create Year.Unit column
new.data.3$Year.Unit<-paste(new.data.3$Year, new.data.3$SMI_Unit,sep=".")

new.data.3$Year.Point<-paste(new.data.3$Year,new.data.3$PointID,sep=".")

#create Year.Point.Visit column
new.data.3$Year.Point.Visit<-paste(new.data.3$Year, new.data.3$PointID, new.data.3$VisitNum, sep=".")

#scale numeric covariates
new.data.3$time.scale<-scale(new.data.3$time2)
new.data.3$temp.scale<-scale(as.numeric(new.data.3$TempF))

new.data.3$Interval.1<-1
new.data.3$Interval.2<-2
new.data.3$Interval.3<-3
new.data.3$Interval.4<-4
new.data.3$Interval.5<-5

new.data.3$Year<-as.factor(as.character(new.data.3$Year))

message("Fixing errors with NWR_code fields.")
names(new.data.3)[names(new.data.3)=="NWR"]<-"NWR_code"
#convert all "RHI" to "SPT" (both Sachuest NWR)
new.data.3$NWR_code[new.data.3$NWR_code == 'RHI'] <- 'SPT'

data<-new.data.3
#remove all data where TotalCount > 10
data$TotalCount<-as.integer(data$TotalCount)
#data<-data[data$TotalCount<10,]

#add RegionName column
data$RegionName<-ifelse(data$RegionNum==1, "1_CoastalMaine",
                    ifelse(data$RegionNum==2, "2_CapeCod-CascoBay",
                           ifelse(data$RegionNum==3, "3_SouthernNewEngland",
                                  ifelse(data$RegionNum==4, "4_LongIsland",
                                         ifelse(data$RegionNum==5, "5_CoastalNewJersey",
                                                ifelse(data$RegionNum==6, "6_DelawareBay",
                                                       ifelse(data$RegionNum==7, "7_CoastalDelmarva",
                                                              ifelse(data$RegionNum==8, "8_EChesapeakeBay",
                                                                     ifelse(data$RegionNum==9,"9_WChesapeakeBay","noRegion")))))))))

data$RegionName<-as.factor(data$RegionName)

#replace all "/" in SMI_Unit
data$SMI_Unit<-gsub("/","-",as.character(data$SMI_Unit))

#remove all Hexagons with NA
data.1<-data[is.na(data$Hexagon)==FALSE,]

##########################
# playback species
#playbackSpecies<-c("CLRA", "KIRA", "VIRA", "SORA", "AMBI", "LEBI", "COMO", "BLRA")

#add TempSum column summing across Min.1 - Min.5
data.1$TempSum<-data.1$Min.1 + data.1$Min.2 + data.1$Min.3+ data.1$Min.4 + data.1$Min.5

data.1$AlphaCode<-as.character(data.1$AlphaCode)

#subset all non-playback species data
otherData<-subset(data.1, c(AlphaCode!="CLRA" & AlphaCode!="KIRA" & AlphaCode!="VIRA" & AlphaCode!="SORA" &
                                        AlphaCode!="AMBI" & AlphaCode!="LEBI" & AlphaCode!="COMO" & AlphaCode!="BLRA"))

#subset all playback data
playbackSpeciesData<-subset(data.1, c(AlphaCode=="CLRA" | AlphaCode=="KIRA" | AlphaCode=="VIRA" | AlphaCode=="SORA" |
                                        AlphaCode=="AMBI" |AlphaCode=="LEBI" |AlphaCode=="COMO" |AlphaCode=="BLRA"))

#subset all playbackSpeciesData with TempSum > 1
tempData.1<-playbackSpeciesData

#remove NAs (using TempSum column)
tempData.2<-tempData.1[is.na(tempData.1$TempSum)==FALSE,]
###################################################################################################
#build fixPlaybackSpecies function

fixPlaybackSpecies<-function(dataIn){
  temp.data<-dataIn
  rangeList<-seq(1,length(row.names(temp.data)))

  results.out<-list()
  for(i in 1:length(rangeList)){
    print(i)

    tempData<-subset(temp.data[rangeList[i],])
    if(tempData$TempSum>1 & tempData$Min.1==1 | is.na(tempData$Min.1)==TRUE){
      tempData$Min.1=1
      tempData$Min.2=0
      tempData$Min.3=0
      tempData$Min.4=0
      tempData$Min.5=0
    }else{
      if(tempData$TempSum>1 & tempData$Min.2==1 | is.na(tempData$Min.2)==TRUE){
        tempData$Min.1=0
        tempData$Min.2=1
        tempData$Min.3=0
        tempData$Min.4=0
        tempData$Min.5=0
      }else{
        if(tempData$TempSum>1 & tempData$Min.3==1 | is.na(tempData$Min.3)==TRUE){
          tempData$Min.1=0
          tempData$Min.2=0
          tempData$Min.3=1
          tempData$Min.4=0
          tempData$Min.5=0
        }else{
          if(tempData$TempSum>1 & tempData$Min.4==1| is.na(tempData$Min.4)==TRUE ){
            tempData$Min.1=0
            tempData$Min.2=0
            tempData$Min.3=0
            tempData$Min.4=1
            tempData$Min.5=0
          }else{
            if(tempData$TempSum>1 & tempData$Min.5==1 | is.na(tempData$Min.5)==TRUE){
              tempData$Min.1=0
              tempData$Min.2=0
              tempData$Min.3=0
              tempData$Min.4=0
              tempData$Min.5=1
            }else{
              tempData$Min.1=tempData$Min.1
              tempData$Min.2=tempData$Min.2
              tempData$Min.3=tempData$Min.3
              tempData$Min.4=tempData$Min.4
              tempData$Min.5=tempData$Min.5
            }
          }}}}

    results.out<-rbind(results.out, tempData)
  }
  return(results.out)
}

###################################################################################################
system.time(PlaybackData<-fixPlaybackSpecies(dataIn=tempData.2))

data.3<-rbind(otherData,PlaybackData)

#
#########################

#create unique "ID" column from NWR_code and

#save formatted data
message("Saving formatted SHARP data as .csv file.")
write.csv(data.3, paste(getwd(),"Data","SHARP_data",saveFileName,sep="/"),row.names=FALSE )

}

#End