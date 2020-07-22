#format raw data and save .csv file
formatRawData<-function(dataIn,saveFileName){

#read in raw data and format
raw.data<-dataIn

new.names<-names(raw.data)

old.names<-c("RegionNum",    "State",        "Hexagon",      "NWR",          "NWR_code",     "NWR_Comp",     "unit_ID",
             "PointID",      "SurveyMethod", "VisitNum",     "SurveyWindow", "SurveyDate",   "Observer",     "AddlObserver",
             "Tide",         "SurveyTime",   "TempF",        "Sky",          "WindSp",       "Noise",        "AlphaCode",
             "Distance",     "DistBand",     "CallType",     "TotalCount",   "Min.1",        "Min.2",        "Min.3",
             "Min.4",        "Min.5",        "BLRA",         "LEBI",         "SORA",         "VIRA",         "KIRA",
             "CLRA",         "AMBI",         "COMO",         "SOSP",         "Outside",      "Comments")

setdiff(new.names, old.names)
###############################################################
#work on test for columnn name accuracy when data is formatted
# namesIn<-names(raw.data)
# 
# #test to make sure names of dataIn columns are correct
# testNames<-function(dataIn){
#   
#   new.data<-dataIn
#   
#   namesIn<-names(new.data)
#   #correct names
#   correct.names<-"wrong"
#   
#   #test for differences
#   test.result<-setdiff(correct.names, namesIn)
#   
#   #output data or error message
#   data.out<- tryCatch({
#     if(identical(test.result, character(0))==TRUE) new.data
#   },error=function(cond2){
#     cond2=message("Error: data is not formatted properly.")
#   })
# 
#   return(data.out)
# }
# 
# data.2<-testNames(dataIn=raw.data)
#########################################################################
data<-raw.data[,c("RegionNum",    "State",        "Hexagon",      "NWR",     "SMI_Unit", "Rest_Unit", "ContSite",
                  "PointID",      "SurveyMethod", "VisitNum",     "SurveyWindow", "SurveyDate",   "Observer",     "AddlObserver",
                  "Tide",         "SurveyTime",   "TempF",        "Sky",          "WindSp",       "Noise",        "AlphaCode",
                  "Distance",     "DistBand",     "CallType",     "TotalCount",   "Min.1",        "Min.2",        "Min.3",
                  "Min.4",        "Min.5",        "BLRA",         "LEBI",         "SORA",         "VIRA",         "KIRA",
                  "CLRA",         "AMBI",         "COMO",         "SOSP",         "Outside",      "Comments")]

#add Month, Year, and Day columns to data
date.data<-data$SurveyDate
date.data<-as.character(date.data)
date.string<-read.table(text=as.character(date.data), sep="/",colClasses = "character")
colnames(date.string)<-c("Month","Day","Year")
date.string$Year<-as.factor(paste("20",date.string$Year,sep=""))
date.string$new.date<-as.character(paste(date.string$Year,date.string$Month,date.string$Day,sep="/"))
date.string$Day.ord <- as.integer(format(as.Date(date.string$new.date, format = "%Y/%m/%d"), "%j"))

head(date.string)
data<-cbind(data,date.string)

#create unit_ID numeric identifier (only have to do this one time)
data<-transform(data,UnitNum=as.numeric(factor(unit_ID)))

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

data$time2<-timeConvertDecimal(DataIn=data)
data$time2 <- as.numeric(as.character(data$time2))

#add a DistCode column
data$DistCode<-ifelse(data$DistBand=="0-50m",0,
                      ifelse(data$DistBand=="51-100m",1,
                             ifelse(data$DistBand==">100m",2,NA)))



#This step should be done on formatted data: create DistCode (combine 0-50m and 51-100m distance bands)
# data$DistCode<-ifelse(data$DistBand=="0-50m",1,ifelse(data$DistBand=="51-100m",1,0))
# 
# data<-subset(data, DistCode==1)


#convert TotalCount to integer and "." to NAs
data$TotalCount[data$TotalCount=="."]<-NA
data$TotalCount<-as.factor(as.character(data$TotalCount))
data$TotalCount<-as.integer(as.character(data$TotalCount))

#subset data by NONE AND NO TARGET SP
no.detections<-subset(data, c(AlphaCode=="NONE" | AlphaCode=="NO TARGET SP"))
no.detections$TotalCount<-0
no.detections$Min.1<-0
no.detections$Min.2<-0
no.detections$Min.3<-0
no.detections$Min.4<-0
no.detections$Min.5<-0

#subset data by all data with detectiuons
detections<-subset(data, c(AlphaCode!="NONE" & AlphaCode!="NO TARGET SP"))

#combine data back together
data.2<-rbind(detections, no.detections)


#convert count data to integers
data.2$Min.1<-as.integer(as.character(data.2$Min.1))
data.2$Min.2<-as.integer(as.character(data.2$Min.2))
data.2$Min.3<-as.integer(as.character(data.2$Min.3))
data.2$Min.4<-as.integer(as.character(data.2$Min.4))
data.2$Min.5<-as.integer(as.character(data.2$Min.5))

#create Year.Unit column
data.2$Year.Unit<-paste(data.2$Year, data.2$unit_ID,sep=".")

data.2$Year.Point<-paste(data.2$Year,data.2$PointID,sep=".")

#create Year.Point.Visit column
data.2$Year.Point.Visit<-paste(data.2$Year, data.2$PointID, data.2$VisitNum, sep=".")

#scale numeric covariates
data.2$time.scale<-scale(data.2$time2)
data.2$temp.scale<-scale(as.numeric(data.2$TempF))

data.2$Interval.1<-1
data.2$Interval.2<-2
data.2$Interval.3<-3
data.2$Interval.4<-4
data.2$Interval.5<-5

data.2$Year<-as.factor(as.character(data.2$Year))


#change unit_ID to SMI_Unit (only when using old data.2)
names(data.2)[names(data.2) == 'unit_ID'] <- 'SMI_Unit'

#add XY coords

#read in coords
xy.coords<-read.csv(paste(getwd(), "Data","SMI_data","SMIUnits_Year_table.csv",sep="/"),header=TRUE)
xy.coords.2<-xy.coords[,c("SMI_Unit", "Longitude","Latitude")]

data.merge<-merge(data.2, xy.coords.2, by=c("SMI_Unit"))

#save formatted data
write.csv(data.merge, paste(getwd(),"Data","SMI_Data",saveFileName,sep="/"),row.names=FALSE )

}

#End