#########################################################################################
#SHARP comparative survey design study - Shriver et al. 2016
#SALS REMAIN - use to finish incomplete analyses
#########################################################################################
#clear environment
rm(list=ls())

getwd()

###########################################################################################
#set working directory (on Z desktop)
setwd("/Users/Zach/Dropbox (ZachTeam)/SMI_Power_SALS_Remain")


#set working directory (on G desktop)
setwd("C:/Users/gshriver/Dropbox/ZachGreg/SMI_Power_SALS_Remain")


#set working directory (on AWS)
setwd("/home/rstudio/Dropbox/SMI_Power_SALS_Remain")
###########################################################################################
install.packages(c("reshape","plyr","unmarked","rjags", "arm", "ggplot2",
                   "RColorBrewer","doParallel",
                   "foreach","dclone","data.table"))

#########################################################################################
#load 'loadPackages.R' source file
source(paste(getwd(),"R_source","loadPackages.R",sep="/"))

#########################################################################################
#test loadPackages function #WORKS
loadPackages()

#########################################################################################
#read in formatted data
data<-read.csv(paste(getwd(),"Data","SMI_data","SMIpts_bird_data_2011-2015_formatted.csv",sep="/"),header=TRUE)

###########################################################################################################
#create folders
createFolders(dataIn=data)


###########################################################################################################
#compile results
compile


#########################################################################################
detectCores()
cl<-makeCluster(7)
registerDoParallel(cl)

#########################################################################################
#HERE'S WHERE YOU GET NEW LIST OF REMAINING SCENARIOS

#read in list of incomplete scenarios
scenarios.toDo<-read.csv("SALS.incomplete_EBF_11-21-16.csv",header=TRUE)
colnames(scenarios.toDo)[1]<-"Scenario"
head(scenarios.toDo)
scenarios.toDo$Scenario<-as.character(scenarios.toDo$Scenario)
#########################################################################################
scenarioList<-c("Full_annual","Full_biennial","Full_triennial","Half_A_annual","Half_A_biennial","Half_A_triennial",
                "Half_B_annual","Half_B_biennial","Half_B_triennial", "Third_A_annual","Third_A_biennial","Third_A_triennial",
                "Third_B_annual","Third_B_biennial","Third_B_triennial", "Third_C_annual","Third_C_biennial","Third_C_triennial")
#########################################################################################
refugeList<-sort(unique(as.character(data$NWR_code)))
refugeList<-"EBF"
visitList<-c(1, 2, 3)
yearList<-c(5,10)
#speciesList<-c("SALS","NESP","SESP","CLRA","WILL","TMO")
declineList<-c("-0.01","-0.05","-0.10","-0.30","-0.50")

#run different scenarios
speciesList<-"SALS"

for(y in 1:length(scenarioList)){
  # foreach(y=1:length(scenarioList),.packages=c("reshape","plyr","unmarked","rjags","arm","ggplot2","RColorBrewer","data.table",
  # 			"doParallel","foreach","dclone")) %dopar% {
  new.scenario<-scenarioList[y]

  for(i in 1:length(refugeList)){
  #   foreach(i=1:length(refugeList),.packages=c("reshape","plyr","unmarked","rjags","arm","ggplot2","RColorBrewer","data.table",
  # 			"doParallel","foreach","dclone")) %dopar% {
    new.refuge<-refugeList[i]

    for(j in 1:length(speciesList)){
  #     foreach(j=1:length(speciesList),.packages=c("reshape","plyr","unmarked","rjags","arm","ggplot2","RColorBrewer","data.table",
  # 			"doParallel","foreach","dclone")) %dopar% {
      new.species<-speciesList[j]

      scenario<-scenarioList[y]
      #choose scenario
      datafilePath<-switch(scenario,
                           "Full_annual"=refugeDataFilepath_full,
                           "Full_biennial"=refugeDataFilepath_full,
                           "Full_triennial"=refugeDataFilepath_full,
                           "Half_A_annual"=refugeDataFilepath_half_A,
                           "Half_A_biennial"=refugeDataFilepath_half_A,
                           "Half_A_triennial"=refugeDataFilepath_half_A,
                           "Half_B_annual"=refugeDataFilepath_half_B,
                           "Half_B_biennial"=refugeDataFilepath_half_B,
                           "Half_B_triennial"=refugeDataFilepath_half_B,
                           "Third_A_annual"=refugeDataFilepath_third_A,
                           "Third_A_biennial"=refugeDataFilepath_third_A,
                           "Third_A_triennial"=refugeDataFilepath_third_A,
                           "Third_B_annual"=refugeDataFilepath_third_B,
                           "Third_B_biennial"=refugeDataFilepath_third_B,
                           "Third_B_triennial"=refugeDataFilepath_third_B,
                           "Third_C_annual"=refugeDataFilepath_third_C,
                           "Third_C_biennial"=refugeDataFilepath_third_C,
                           "Third_C_triennial"=refugeDataFilepath_third_C)

      savefilePathAbundance<-switch(scenario,
                                    "Full_annual"=refugeFilepath_full_annual,
                                    "Full_biennial"=refugeFilepath_full_biennial,
                                    "Full_triennial"=refugeFilepath_full_triennial,
                                    "Half_A_annual"=refugeFilepath_half_A_annual,
                                    "Half_A_biennial"=refugeFilepath_half_A_biennial,
                                    "Half_A_triennial"=refugeFilepath_half_A_triennial,
                                    "Half_B_annual"=refugeFilepath_half_B_annual,
                                    "Half_B_biennial"=refugeFilepath_half_B_biennial,
                                    "Half_B_triennial"=refugeFilepath_half_B_triennial,
                                    "Third_A_annual"=refugeFilepath_third_A_annual,
                                    "Third_A_biennial"=refugeFilepath_third_A_biennial,
                                    "Third_A_triennial"=refugeFilepath_third_A_triennial,
                                    "Third_B_annual"= refugeFilepath_third_B_annual,
                                    "Third_B_biennial"= refugeFilepath_third_B_biennial,
                                    "Third_B_triennial"= refugeFilepath_third_B_triennial,
                                    "Third_C_annual"= refugeFilepath_third_C_annual,
                                    "Third_C_biennial"= refugeFilepath_third_C_biennial,
                                    "Third_C_triennial"= refugeFilepath_third_C_triennial)
      savefilePathPower<-switch(scenario,
                                "Full_annual"=bayesPwrFilepath_full_annual,
                                "Full_biennial"=bayesPwrFilepath_full_biennial,
                                "Full_triennial"=bayesPwrFilepath_full_triennial,
                                "Half_A_annual"=bayesPwrFilepath_half_A_annual,
                                "Half_A_biennial"=bayesPwrFilepath_half_A_biennial,
                                "Half_A_triennial"=bayesPwrFilepath_half_A_triennial,
                                "Half_B_annual"=bayesPwrFilepath_half_B_annual,
                                "Half_B_biennial"=bayesPwrFilepath_half_B_biennial,
                                "Half_B_triennial"=bayesPwrFilepath_half_B_triennial,
                                "Third_A_annual"=bayesPwrFilepath_third_A_annual,
                                "Third_A_biennial"=bayesPwrFilepath_third_A_biennial,
                                "Third_A_triennial"=bayesPwrFilepath_third_A_triennial,
                                "Third_B_annual"= bayesPwrFilepath_third_B_annual,
                                "Third_B_biennial"= bayesPwrFilepath_third_B_biennial,
                                "Third_B_triennial"= bayesPwrFilepath_third_B_triennial,
                                "Third_C_annual"= bayesPwrFilepath_third_C_annual,
                                "Third_C_biennial"= bayesPwrFilepath_third_C_biennial,
                                "Third_C_triennial"= bayesPwrFilepath_third_C_triennial)

      message(paste("Getting data from ",refugeList[i], " for ",speciesList[j]," and running abundance models in 'unmarked'.",sep=""))
      #read in refuge data
      if(speciesList[j]=="TMO"){
        refuge.data<-read.csv(paste(datafilePath,paste(refugeList[i],"data.tmo.csv",sep="."),sep="/"),header=TRUE)
      }else{
        refuge.data<-read.csv(paste(datafilePath,paste(refugeList[i],"data.csv",sep="."),sep="/"),header=TRUE)
      }

      mod.1<-try(getAbundance(dataIn=refuge.data, speciesName=speciesList[j],filePath=savefilePathAbundance))

      for(q in 1:length(yearList)){
  #       foreach(q=1:length(yearList),.packages=c("reshape","plyr","unmarked","rjags","arm","ggplot2","RColorBrewer","data.table",
  # 				"doParallel","foreach","dclone")) %dopar% {
        new.year<-yearList[q]

        for(t in 1:length(visitList)){
  #         foreach(t=1:length(visitList),.packages=c("reshape","plyr","unmarked","rjags","arm","ggplot2","RColorBrewer","data.table",
  # 					"doParallel","foreach","dclone")) %dopar% {
          new.visit<-visitList[t]

          #for(z in 1:length(declineList)){
            foreach(z=1:length(declineList),.packages=c("reshape","plyr","unmarked","rjags","arm","ggplot2","RColorBrewer","data.table",
  					"doParallel","foreach","dclone")) %dopar% {
            new.decline<-declineList[z]

            #now decide whether to run JAGS or NOT based on completed list
            new.run<-paste(new.species,"_",new.scenario,"_",new.refuge,"_",new.decline,"_","decline","_",new.visit,
                           "_","visits","_",new.year,"_","years",sep="")

          try(if(new.run %in% scenarios.toDo$Scenario==TRUE){
          #step 2: simulate data for given trends and run Bayesian power analysis in JAGS
          message(paste("Simulating new data and running power analysis in JAGS for ",new.run,sep=""))
          print(new.run)
          try(runJAGSmodelUnits_Visit_allYears(dataIn=refuge.data, nsim=100, modIn=mod.1,declineRate=declineList[z],nyrs=yearList[q],
                                               nvisits=visitList[t],speciesName=speciesList[j],filePath=savefilePathPower,scenario=scenario))
                  })
            }
        }
      }
    }
  }
}

#########################################################################################
#subset and save data (Full dataset) by NWR
getRefugeData(dataIn=data, filePath=paste(getwd(),"Data","Data_refuge_full",sep="/"))
#########################################################################################
#randomly divide units in half and save files
divideUnitsHalf(dataIn=data)

#########################################################################################
#randomly divide units in half and save files
divideUnitsThirds(dataIn=data)

##################################################################################################################
#how many units in each refuge
refugeList<-sort(unique(as.character(data$NWR_code)))
table.out<-list()
for(i in 1:length(refugeList)){
  new.data<-subset(data, NWR_code==refugeList[i])
  refugeName<-unique(new.data$NWR_code)
  numUnits<-length(unique(new.data$SMI_Unit))

  new.df<-data.frame(NWR=refugeName, numUnits=numUnits)
  table.out<-rbind(table.out, new.df)

}



##################################################################################################################
#read in group 1 and 2 data
length(unique(data$SMI_Unit))
group.1.data<-read.csv(paste(getwd(),"Data","Data_Half","SMI.group1.compiled.csv",sep="/"),header=TRUE)
length(unique(as.character(group.1.data$SMI_Unit)))

group.2.data<-read.csv(paste(getwd(),"Data","Data_Half","SMI.group2.compiled.csv",sep="/"),header=TRUE)
length(unique(as.character(group.2.data$SMI_Unit)))

##################################################################################################################
#use getRefugeData function on each of halved datasets
getRefugeData(dataIn=group.1.data, filePath=paste(getwd(),"Data","Data_refuge_half_A",sep="/"))

getRefugeData(dataIn=group.2.data, filePath=paste(getwd(),"Data","Data_refuge_half_B",sep="/"))

##################################################################################################################
#read in group 1, 2, and 3 data Thirds

group.1.data.thirds<-read.csv(paste(getwd(),"Data","Data_Thirds","SMI_data_2011-2015_third1.csv",sep="/"),header=TRUE)
length(unique(group.1.data.thirds$SMI_Unit))

group.2.data.thirds<-read.csv(paste(getwd(),"Data","Data_Thirds","SMI_data_2011-2015_third2.csv",sep="/"),header=TRUE)
length(unique(group.2.data.thirds$SMI_Unit))

group.3.data.thirds<-read.csv(paste(getwd(),"Data","Data_Thirds","SMI_data_2011-2015_third3.csv",sep="/"),header=TRUE)
length(unique(group.3.data.thirds$SMI_Unit))

##################################################################################################################
#use getRefugeData function on each of datasets divided into thirds
getRefugeData(dataIn=group.1.data.thirds, filePath=paste(getwd(),"Data","Data_refuge_third_A",sep="/"))

getRefugeData(dataIn=group.2.data.thirds, filePath=paste(getwd(),"Data","Data_refuge_third_B",sep="/"))

getRefugeData(dataIn=group.3.data.thirds, filePath=paste(getwd(),"Data","Data_refuge_third_C",sep="/"))

##################################################################################################################

cl<-makeCluster(32)
registerDoSNOW(cl)


detectCores()
cl<-makeCluster(40)
registerDoParallel(cl)


#to stop parallel tasks
stopCluster(cl)
rm(cl)

##########TEST############
refugeList<-"RHC"
speciesList<-c("NESP")
visitList<-c(1,2,3)
declineList<-c("-0.25","-0.50")
yearList<-c(5,10)

##########TEST############


#abundance models optimize for SALS
BMH.data<-read.csv(paste(refugeDataFilepath_full,paste("BMH","data.csv",sep="."),sep="/"),header=TRUE)
BMH.data.tmo<-read.csv(paste(refugeDataFilepath,paste("BMH","data.tmo.csv",sep="."),sep="/"),header=TRUE)
BMH.data.tmo$Year<-as.integer(as.character(BMH.data.tmo$Year))

mod.1<-getAbundance(dataIn=BMH.data,speciesName="SALS",filePath=savefilePathAbundance)

umf<-makeGMMumf(dataIn=RHC.data, species="NESP")
mod.1=gmultmix(~Year+UnitNum, ~1, ~VisitNum, data=umf,mixture="NB")
getModelOutput(dataIn=RHC.data, modelIn=mod.1, speciesName="NESP")

############################################################################
#abundance models Third_A_triennial
BMH.data<-read.csv(paste(refugeDataFilepath_third_A,paste("BMH","data.csv",sep="."),sep="/"),header=TRUE)
BMH.data.tmo<-read.csv(paste(refugeDataFilepath,paste("BMH","data.tmo.csv",sep="."),sep="/"),header=TRUE)
BMH.data.tmo$Year<-as.integer(as.character(BMH.data.tmo$Year))

mod.1<-getAbundance(dataIn=BMH.data,speciesName="SALS",filePath=refugeFilepath_third_A_triennial)

umf<-makeGMMumf(dataIn=RHC.data, species="NESP")
mod.1=gmultmix(~Year+UnitNum, ~1, ~VisitNum, data=umf,mixture="NB")
getModelOutput(dataIn=RHC.data, modelIn=mod.1, speciesName="NESP")



#set up paralellized cores for running JAGS NWR

detectCores()
cl<-makeCluster(4)
registerDoParallel(cl)

#create lists
declineList<-c("-0.10","-0.15","-0.20","-0.25","-0.30","-0.35","-0.40","-0.45","-0.50")
refugeList<-sort(unique(as.character(data$NWR_code)))
speciesList<-c("SALS")

#foreach(z=1:length(declineList),.packages=c("reshape","plyr","unmarked","rjags","arm","ggplot2","RColorBrewer","data.table")) %dopar% {

  print(z)
  for(i in 1:length(refugeList)){
  #foreach(i=1:length(refugeList),.packages=c("reshape","plyr","unmarked","rjags","arm","ggplot2","RColorBrewer","data.table")) %dopar% {
    for(j in 1:length(speciesList)){

      #read in region data
      message(paste("Getting data from ",refugeList[i], " for ",speciesList[j]," and running abundance models in 'unmarked'.",sep=""))

      if(speciesList[j]=="TMO"){
        refuge.data<-read.csv(paste(refugeDataFilepath,paste(refugeList[i],"data.tmo.csv",sep="."),sep="/"),header=TRUE)
      }else{
        refuge.data<-read.csv(paste(refugeDataFilepath,paste(refugeList[i],"data.csv",sep="."),sep="/"),header=TRUE)
      }

      mod.all<-try(getAbundance(dataIn=refuge.data, speciesName=speciesList[j]))

      for(z in 1:length(declineList)){


      #step 2: simulate data for given trends and run Bayesian power analysis in JAGS
      message("Simulating new data and running power analysis in JAGS.")
      try(runJAGSmodelUnits(dataIn=refuge.data, nsim=3, modIn=mod.all,declineRate=declineList[z],nyrs=5,nvisits=3,speciesName=speciesList[j]))
    }
  }
}
##################################################################################################################
##################################################################################################################
  #Windows you fucking piece of shit!!
  dat=2
  res=5
  Rmax=NA
out=10
  saveFilepath<-paste(getwd(),"Results","SMI_Full","Power_Bayes","SALS","3_SouthernNewEngland",paste("SMI_Full_", "Power","_","SALS","_","3_SouthernNewEngland" ,"_","-0.10", "decline_","5","_years", sep=''),sep="/")

  dput( list(data=dat, model=res,R=Rmax, mcs=out),
        file=paste(saveFilepath, paste("JAGS_","SALS","_","3_SouthernNewEngland","_", "156", "_", "-0.10", "decline_", "5","_years","1",".R", sep=''),sep="/")   )
  ##################################################################################################################

#test createFolders #WORKS
createFolders(dataIn=data)

#########################################################################################
#test getRefugeData(dataIn) #WORKS
getHexagonData(dataIn=data)

#########################################################################################
#test getRefugeData(dataIn) #WORKS
getRefugeData(dataIn=SMI.data)

#########################################################################################
#test getRegionData(dataIn) #WORKS
getRegionData(dataIn=SMI.data)

#########################################################################################
#test getStateData(dataIn) #WORKS
getStateData(dataIn=data)

#########################################################################################
#divideUnitsHalf #WORKS
divideUnitsHalf(dataIn=SMI.data)

#########################################################################################
#divideUnitsThirds #WORKS
divideUnitsThirds(dataIn=data)

#########################################################################################
#get some basic summaries from data

#get list of years
yearList<-sort(unique(data$Year))

#number of years in data (n)
nYears<- length(yearList) #5

#get list regions
regionList<-sort(unique(as.character(data$RegionName)))

#get list of refuges
refugeList<-sort(unique(as.character(data$NWR_code)))

#count of regfuges
nRefuges<-length(refugeList)

#list of units
unitList<-sort(unique(data$SMI_Unit))

#count of units
nUnits<-length(unitList) #123 units

#count of Hexagons
hexList<-sort(unique(data$Hexagon))
length(hexList)

#list of points
pointList<-sort(unique(data$PointID))
nPoints<-length(pointList) #1172unitList

#########################################################################################
#test makeGMMumf #WORKS
region2.data<-read.csv(paste(regionDataFilepath,"2_CapeCod-CascoBay.data.csv",sep="/"),header=TRUE)

region1.SMI.data<-subset(region1.data, is.na(SMI_Unit)==FALSE)
unitList<-sort(unique(as.character(region1.SMI.data$SMI_Unit)))

head(test.umf)
#########################################################################################
#test runUnmarked #WORKS
mod.test<-runUnmarkedAllYearsRegion(dataIn=region2.data,speciesName="SALS")
mod.test<-getAbundanceRegion(dataIn=region2.data,speciesName="SALS")

coef(mod.test)

try(runJAGSmodelNWR(dataIn=region2.data, nsim=2, modIn=mod.test,declineRate=declineList[1],nyrs=5,nvisits=3,speciesName=speciesList[1]))

#########################################################################################
#look at Beta coefficients
umf<-makeGMMumf(dataIn=region2.data,species="SALS")

umf.test<-makeGMMumf(dataIn=region2.data, species="SALS")

new.umf(make)
gmultmix(~Year-1, ~1, ~VisitNum-1, data=umf,mixture="NB")

#########################################################################################
#test runUnmarkedNWR #WORKS

NWR.1.data<-subset(SMI.data, NWR_code=="BMH")
mod.test<-runUnmarkedNWR(dataIn=NWR.1.data,speciesName="SALS")

mod.test

#########################################################################################
#test getModelOutput #WORKS
getModelOutputNWR(dataIn=NWR.1.data, modelIn=mod.test, speciesName="SALS")

#########################################################################################
#test runUnmarkedAllYearsNWR #WORKS

NWR.1.data<-subset(SMI.data, NWR_code=="BMH")
mod.test.allyears<-runUnmarkedAllYearsNWR(dataIn=NWR.1.data,speciesName="SALS")

mod.test.allyears

#########################################################################################
#test getModelOutputAllYearsNWR #WORKS
getModelOutputAllYearsNWR(dataIn=NWR.1.data, modelIn=mod.test.allyears, speciesName="SALS")

#########################################################################################
#loop through and get all refuge-level abundance estaimates

detectCores()
cl<-makeCluster(8)
registerDoParallel(cl)

refugeList<-sort(unique(as.character(SMI.data$NWR_code)))
speciesList<-c("SALS")

#use foreach() to parallelize
foreach(i=1:length(refugeList),.packages=c("reshape","plyr","unmarked","rjags","arm","ggplot2","RColorBrewer"))  %dopar% {
  for(j in 1:length(speciesList)){

    if(speciesList[j]=="TMO"){
      NWR.data<-read.csv(paste(refugeDataFilepath,paste(refugeList[i],"data.tmo.csv",sep="."),sep="/"),header=TRUE)
    }else{
      NWR.data<-read.csv(paste(refugeDataFilepath,paste(refugeList[i],"data.csv",sep="."),sep="/"),header=TRUE)
    }

    try(getAbundanceNWR(dataIn=NWR.data, speciesName=speciesList[j]))

  }
}



#########################################################################################
#test getModelOutput #WORKS
getModelOutput(dataIn=BLK.data, modelIn=mod.test, speciesName="SALS")

#########################################################################################
#test runUnmarkedAllYears #WORKS
mod.test.all.years<-runUnmarkedAllYears(dataIn=BLK.data,speciesName="SALS")

mod.test
#########################################################################################
#test getModelOutputAllYears #WORKS
getModelOutputAllYears(dataIn=BMH.data, modelIn=mod.test.all.years, speciesName = "SALS")

#########################################################################################
#test runUnmarkedRegion #WORKS
region2.data<-read.csv(paste(regionDataFilepath,"2_CapeCod-CascoBay.data.csv",sep="/"),header=TRUE)

mod.test<-runUnmarkedRegion(dataIn=region5.data,speciesName="SALS")

mod.test
unique(predict(mod.test, type="lambda"))

#########################################################################################
#test getModelOutputRegion #WORKS
getModelOutputRegion(dataIn=region5.data, modelIn=mod.test, speciesName="SALS")

#########################################################################################
#test runUnmarkedAllYearsRegion #WORKS
mod.test.all.years<-runUnmarkedAllYearsRegion(dataIn=region1.data,speciesName="SALS")

mod.test.all.years
unique(predict(mod.test, type="lambda"))

#########################################################################################
#test getModelOutputAllYears #WORKS
getModelOutputAllYearsRegion(dataIn=region1.data, modelIn=mod.test.all.years, speciesName = "SALS")

#########################################################################################
#test runUnmarkedRegionHexagon #
region5.data<-read.csv(paste(regionDataFilepath,"5_CoastalNewJersey.data.csv",sep="/"),header=TRUE)

mod.test<-runUnmarkedRegionHexagon(dataIn=region5.data,speciesName="SALS")

mod.test
unique(predict(mod.test, type="lambda"))

#########################################################################################
#test getModelOutputRegionHexagon #
getModelOutputRegionHexagon(dataIn=region5.data, modelIn=mod.test, speciesName="SALS")

#########################################################################################
#test runUnmarkedAllYearsRegionHexagon #WORKS
mod.test.all.years<-runUnmarkedAllYearsRegion(dataIn=region1.data,speciesName="SALS")

mod.test.all.years
unique(predict(mod.test, type="lambda"))

#########################################################################################
#test getModelOutputAllYearsHexagon #WORKS
getModelOutputAllYearsRegion(dataIn=region1.data, modelIn=mod.test.all.years, speciesName = "SALS")

#########################################################################################
#########################################################################################
#test runUnmarkedHexagon #WORKS
hex.data<-read.csv(paste(hexagonDataFilepath,"68731.data.csv",sep="/"),header=TRUE)

mod.test<-runUnmarkedHexagon(dataIn=hex.data,speciesName="SALS")

mod.test
unique(predict(mod.test, type="lambda"))

#########################################################################################
#test getModelOutputHexagon #WORKS
getModelOutputHexagon(dataIn=hex.data, modelIn=mod.test, speciesName="SALS")

#########################################################################################
#test runUnmarkedAllYearsRegionHexagon #WORKS
mod.test.all.years<-runUnmarkedAllYearsHexagon(dataIn=hex.data,speciesName="SALS")

mod.test.all.years
unique(predict(mod.test.all.years, type="lambda"))

new.df<-data.frame(Hexagon="test",unique(predict(mod.test.all.years, type="lambda"))
)
#########################################################################################
#test getModelOutputAllYearsHexagon #WORKS
getModelOutputAllYearsHexagon(dataIn=hex.data, modelIn=mod.test.all.years, speciesName = "SALS")

#########################################################################################
#test getAbundanceHexagon  #WORKS
getAbundanceHexagon(dataIn=hex.data, speciesName = "SALS")

#########################################################################################
#test parallelized loop to getAbundanceHexagon #WORKS

detectCores()
cl<-makeCluster(4)
registerDoParallel(cl)

hexList<-sort(unique(as.character(data$Hexagon)))
speciesList<-c("TMO","SALS","SESP","NESP","CLRA","WILL")

#use foreach() to parallelize
foreach(j=1:length(speciesList),.packages=c("reshape","plyr","unmarked","rjags","arm","ggplot2","RColorBrewer"))  %dopar% {
  #for(j in 1:length(speciesList)){
  for(i in 1:length(hexList)){

    if(speciesList[j]=="TMO"){
      hex.data<-read.csv(paste(hexagonDataFilepath,paste(hexList[i],"data.tmo.csv",sep="."),sep="/"),header=TRUE)
    }else{
      hex.data<-read.csv(paste(hexagonDataFilepath,paste(hexList[i],"data.csv",sep="."),sep="/"),header=TRUE)
    }

    try(getAbundanceHexagon(dataIn=hex.data, speciesName=speciesList[j]))

  }
}

#########################################################################################
#function to compile plots (All years average)
new.output<-list()
for(j in 1:length(speciesList)){
  for(i in 1:length(hexList)){

    new.data<-tryCatch({
      cond1<-read.csv(paste(hexagonFilepath_full,speciesList[j],hexList[i],paste(speciesList[j],".abundance.by.NWR.csv",sep=""),sep="/" ),header=TRUE)
      cond1
      },error=function(cond2){
          cond2=data.frame(Hexagon=hexList[i], Predicted=NA, SE=NA, lower=NA, upper=NA)
          cond2
        })

    new.data.2<-data.frame(Species=speciesList[j], new.data)
    new.output<-rbind(new.output, new.data.2)

  }
  write.csv(new.output, file=paste(hexagonFilepath_full,speciesList[j], paste(speciesList[j],"_all.abun.est.by.hexagon.csv",sep=""),sep="/"),row.names = FALSE)
}
write.csv(new.output, file=paste(hexagonFilepath_full, paste("All_spp_","all.abun.est.by.hexagon.csv",sep=""),sep="/"),row.names = FALSE)

#########################################################################################
#plot abundance by species and hexagons

#read in abun estimates
hex.abun<-read.csv(paste(hexagonFilepath_full, "All_spp_all.abun.est.by.hexagon.csv",sep="/"),header=TRUE)
head(hex.abun, 20)

#get region lookup info from data
region.info<-unique(data[,c("Hexagon","State","RegionName")])

hex.abun.merge<-merge(region.info,hex.abun, by=c("Hexagon"))
hex.abun.merge$mean<-ifelse(is.na(hex.abun.merge$Predicted)==TRUE,0,hex.abun.merge$Predicted)
hex.abun.merge$se2<-ifelse(is.na(hex.abun.merge$SE)==TRUE,0,hex.abun.merge$SE)

hex.abun.merge$State<-factor(hex.abun.merge$State, levels=c("ME","NH","MA",
                                                               "RI","CT","NY",
                                                               "NJ","DE", "MD","VA"))

#########################################################################################
#loop to get species means by RegionName
new.out<-list()
for(i in 1:length(speciesList)){
  new.data<-subset(hex.abun.merge, Species==speciesList[i])

  hex.abun.summary<-summaryFunction(dataIn=new.data, factor="RegionName",
                                    response="Predicted")
  hex.abun.summary<-data.frame(Species=speciesList[i],hex.abun.summary)

  new.out<-rbind(new.out, hex.abun.summary)
}

write.csv(new.out,file=paste(hexagonFilepath_full,"hex.abun.Region.summary.csv",sep="/"),row.names=FALSE)

#########################################################################################
#by Region
#read in data
new.summary<-read.csv(paste(hexagonFilepath_full,"hex.abun.Region.summary.csv",sep="/"),header=TRUE)

#plot data
new.plot.region<-ggplot(new.summary)+
  aes(x=RegionName, y=mean,ymin=mean-SE, ymax=mean+SE)+
  geom_errorbar(width=0, size=1, col="steelblue4")+
  geom_point(aes(fill=Species))+
  labs(x="State",y="Estimated abundance")
new.plot.region
new.plot.region.2<-new.plot.region+facet_wrap(~Species)
new.plot.3<-new.plot+facet_wrap(~Species)

#loop to get species means by RegionName
new.out<-list()
for(i in 1:length(speciesList)){
  new.data<-subset(hex.abun.merge, Species==speciesList[i])

  hex.abun.summary<-summaryFunction(dataIn=new.data, factor="RegionName",
                                    response="Predicted")
  hex.abun.summary<-data.frame(Species=speciesList[i],hex.abun.summary)

  new.out<-rbind(new.out, hex.abun.summary)
}

write.csv(new.out,file=paste(hexagonFilepath_full,"hex.abun.Region.summary.csv",sep="/"),row.names=FALSE)

#########################################################################################
#by Region
#read in data
new.summary<-read.csv(paste(hexagonFilepath_full,"hex.abun.Region.summary.csv",sep="/"),header=TRUE)

#plot data
new.plot.region<-ggplot(new.summary)+
  aes(x=RegionName, y=mean,ymin=mean-SE, ymax=mean+SE)+
  geom_errorbar(width=0, size=1, col="steelblue4")+
  geom_point(aes(fill=Species))+
  labs(x="State",y="Estimated abundance")
new.plot.region
new.plot.region.2<-new.plot.region+facet_wrap(~Species)
new.plot.3<-new.plot+facet_wrap(~Species)

#########################################################################################

#by State
new.plot<-ggplot(hex.abun.merge)+
  aes(x=State, y=Predicted)+
  geom_boxplot(aes(fill=State))+
  labs(x="State",y="Estimated abundance")

new.plot.2<-new.plot+facet_wrap(~Species)
new.plot.3<-new.plot+facet_wrap(~Species)


#save plot of abundance estimates
ggsave(new.plot, filename=paste("Boxplot_abun.hexagon.by.state.pdf",sep="."),path=hexagonFilepath_full, width=11,height=8.5, limitsize=FALSE)
ggsave(new.plot.2, filename=paste("Boxplot_abun.hexagon.by.state.and.species.pdf",sep="."),path=hexagonFilepath_full, width=11,height=8.5, limitsize=FALSE)
ggsave(new.plot.2, filename=paste("Boxplot_abun.hexagon.by.state.and.species.pdf",sep="."),path=hexagonFilepath_full, width=11,height=8.5, limitsize=FALSE)

#########################################################################################
#function to compile plots (Hexagon means by year)
new.output<-list()
for(j in 1:length(speciesList)){
  for(i in 1:length(hexList)){

    new.data<-tryCatch({
      cond1<-read.csv(paste(hexagonFilepath_full,speciesList[j],hexList[i],paste(speciesList[j],".abundance.by.year.and.unit.csv",sep=""),sep="/" ),header=TRUE)#change to ". . .year.and.hexagon.csv"
      cond1
    },error=function(cond2){
      cond2=data.frame(Hexagon=hexList[i], Predicted=NA, SE=NA, lower=NA, upper=NA)
      cond2
    })

    new.data.2<-data.frame(Species=speciesList[j], new.data)
  }
  new.output<-rbind(new.output, new.data.2)
  write.csv(new.output, file=paste(hexagonFilepath_full,speciesList[j], paste(speciesList[j],"_all.abun.est.by.year.and.hexagon.csv",sep=""),sep="/"),row.names = FALSE)
}
write.csv(new.output, file=paste(hexagonFilepath_full, paste("All_spp_","all.abun.est.by.year.and.hexagon.csv",sep=""),sep="/"),row.names = FALSE)

#########################################################################################
#plot abundance by species and hexagons

#read in abun estimates
hex.abun.year<-read.csv(paste(hexagonFilepath_full, "All_spp_all.abun.est.by.year.and.hexagon.csv",sep="/"),header=TRUE)
head(hex.abun.year, 20)


#get region lookup info from data
region.info<-data[,c("Hexagon","State","RegionName")]


#########################################################################################
#test getModelOutputRegionHexagon #
getModelOutputStateHexagon(dataIn=region5.data, modelIn=mod.test, speciesName="SALS")

#########################################################################################
#test runUnmarkedAllYearsRegionHexagon #WORKS
mod.test.all.years<-runUnmarkedAllYearsRegion(dataIn=region1.data,speciesName="SALS")

mod.test.all.years
unique(predict(mod.test, type="lambda"))

#########################################################################################
#test getModelOutputAllYearsHexagon #WORKS
getModelOutputAllYearsRegion(dataIn=region1.data, modelIn=mod.test.all.years, speciesName = "SALS")

#########################################################################################

#test runUnmarkedRegionAllYearsHexagon #
region5.data<-read.csv(paste(regionDataFilepath,"5_CoastalNewJersey.data.csv",sep="/"),header=TRUE)

mod.test<-runUnmarkedAllYearsRegionHexagon(dataIn=region5.data,speciesName="SALS")

mod.test
unique(predict(mod.test, type="lambda"))

#########################################################################################
#test getModelOutputRegionAllYearsHexagon #
getModelOutputAllYearsRegionHexagon(dataIn=region5.data, modelIn=mod.test, speciesName="SALS")

#########################################################################################
#test getAbundance by Hexagon #WORKS

#for loop to get all abundance estimates

refugeList
speciesList<-c("SALS")
for(j in 1:length(speciesList)){
  for(i in 1:length(hexList)){

    if(speciesList[j]=="TMO"){
      hexagon.data<-read.csv(paste(hexagonDataFilepath,paste(hexList[i],"data.tmo.csv",sep="."),sep="/"),header=TRUE)
    }else{
      hexagon.data<-read.csv(paste(hexagonDataFilepath,paste(hexList[i],"data.csv",sep="."),sep="/"),header=TRUE)
    }

    try(getAbundanceHexagon(dataIn=hexagon.data, speciesName=speciesList[j]))

  }
}

#########################################################################################
#test getAbundance #WORKS (Need PatchID for all points to use this!!!)
getAbundance(dataIn=BLK.data,speciesName = "SALS")

#for loop to get all abundance estimates

refugeList
speciesList<-c("SALS")
for(j in 1:length(speciesList)){
  for(i in 1:length(refugeList)){

    if(speciesList[j]=="TMO"){
      refuge.data<-read.csv(paste(refugeDataFilepath,paste(refugeList[i],"data.tmo.csv",sep="."),sep="/"),header=TRUE)
    }else{
      refuge.data<-read.csv(paste(refugeDataFilepath,paste(refugeList[i],"data.csv",sep="."),sep="/"),header=TRUE)
    }

    try(getAbundance(dataIn=refuge.data, speciesName=speciesList[j]))

  }
}
#########################################################################################
#test getAbundanceRegion #WORKS
getAbundanceRegion(dataIn=region1.data,speciesName = "SALS")

#########################################################################################
#for loop to get all Region abundance estimates

detectCores()
cl<-makeCluster(4)
registerDoParallel(cl)

regionList
speciesList<-c("TMO","SALS","SESP","NESP","CLRA","WILL")

#use foreach() to parallelize
foreach(j=1:length(speciesList),.packages=c("reshape","plyr","unmarked","rjags","arm","ggplot2","RColorBrewer"))  %dopar% {
#for(j in 1:length(speciesList)){
  for(i in 1:length(regionList)){

    if(speciesList[j]=="TMO"){
      region.data<-read.csv(paste(regionDataFilepath,paste(regionList[i],"data.tmo.csv",sep="."),sep="/"),header=TRUE)
    }else{
      region.data<-read.csv(paste(regionDataFilepath,paste(regionList[1],"data.csv",sep="."),sep="/"),header=TRUE)
    }

    try(getAbundanceRegion(dataIn=region.data, speciesName=speciesList[j]))

  }
}

#########################################################################################
#test dataSimFullPanel #WORKS
test.sim.data<-dataSimFullPanel(dataIn = BMH.data, modIn=mod.test, declineRate="-0.50",nyrs=10, nvisits=1)
test.sim.data

###################################################################################
#POWER ANALYSIS 2. Using Bayesian power anlayses with 'rjags' - NO Spatial Autocorrelation
###################################################################################
#Write JAGS model NO spatial autocorrelation term (Unit-level)

cat("
    model {

    # Noninformative Prior distributions
    p0 ~ dunif(0,1)
    alpha0 <- logit(p0)
    beta0 ~ dnorm(0,2)

    for(n in 1:nVisits){
    alpha1[n] ~ dnorm(0, 0.01) #fixed effects of visit on det
    }

    for(h in 1:nYears){
    beta1[h] ~ dnorm(0, 0.01) #fixed effects of year on abun
    }

    for(g in 1:nRefuges){
    beta2[g] ~ dnorm(0, 0.011) #fixed effects of NWR on abun
    gamma[g]~dunif(0,5)

    }

    for(i in 1:M){ # Loop over sites
    # Conditional multinomial cell probabilities
    pi[i,1] <- p[i]
    pi[i,2] <- p[i]*(1-p[i])
    pi[i,3] <- p[i]*(1-p[i])*(1-p[i])
    pi[i,4] <- p[i]*(1-p[i])*(1-p[i])*(1-p[i])
    pi[i,5] <- p[i]*(1-p[i])*(1-p[i])*(1-p[i])*(1-p[i])
    pi0[i] <- 1 - (pi[i,1] + pi[i,2] + pi[i,3] + pi[i,4] + pi[i,5])
    pcap[i] <- 1 - pi0[i]
    for(j in 1:5){
    pic[i,j] <- pi[i,j] / pcap[i]
    }

    # logit-linear model for detection: with visit and observer effects
    logit(p[i]) <- alpha0 + alpha1[visit[i]]

    #Abundance model for Year 1
    log(lambda[i])<- beta0  + beta2[unit[i]]

    y[i,1] ~ dbin(pcap[i], N[i,1])
    N[i,1] ~ dpois(lambda[i])

    #Abundance for years with declining trend
    for (t in 2:nYears){
    y[i,t] ~ dbin(pcap[i], N[i,t])
    N[i,t]~ dpois(N[i, t-1]*gamma[unit[i]])
    }#t
    }#i


    }
    ",fill=TRUE, file="JAGS.model.unit.txt")

###################################################################################
#Write JAGS model NO spatial autocorrelation term (Refuge-level)

cat("
    model {

    # Noninformative Prior distributions
    p0 ~ dunif(0,1)
    alpha0 <- logit(p0)
    beta0 ~ dnorm(0,2)

    for(n in 1:nVisits){
    alpha1[n] ~ dnorm(0, 0.01) #fixed effects of year on abun
    }

    for(h in 1:nYears){
    beta1[h] ~ dnorm(0, 0.01) #fixed effects of year on abun
    }

    for(g in 1:nUnits){
    beta2[g] ~ dnorm(0, 0.011) #fixed effects of NWR on abun
    gamma[g]~dunif(0,5)

    }

    for(i in 1:M){ # Loop over sites
    # Conditional multinomial cell probabilities
    pi[i,1] <- p[i]
    pi[i,2] <- p[i]*(1-p[i])
    pi[i,3] <- p[i]*(1-p[i])*(1-p[i])
    pi[i,4] <- p[i]*(1-p[i])*(1-p[i])*(1-p[i])
    pi[i,5] <- p[i]*(1-p[i])*(1-p[i])*(1-p[i])*(1-p[i])
    pi0[i] <- 1 - (pi[i,1] + pi[i,2] + pi[i,3] + pi[i,4] + pi[i,5])
    pcap[i] <- 1 - pi0[i]
    for(j in 1:5){
    pic[i,j] <- pi[i,j] / pcap[i]
    }

    # logit-linear model for detection: with visit and observer effects
    logit(p[i]) <- alpha0 + alpha1[visit[i]]

    #Abundance model for Year 1
    log(lambda[i])<- beta0  + beta2[unit[i]]

    y[i,1] ~ dbin(pcap[i], N[i,1])
    N[i,1] ~ dpois(lambda[i])

    #Abundance for years with declining trend
    for (t in 2:nYears){
    y[i,t] ~ dbin(pcap[i], N[i,t])
    N[i,t]~ dpois(N[i, t-1]*gamma[unit[i]])
    }#t
    }#i


    }
    ",fill=TRUE, file="JAGS.model.unit.txt")

#########################################################################################
#RUN Power analysis in with JAGS model "model.txt'
#########################################################################################
#test runJAGSmodel function
runJAGSmodelUnits(dataIn=BMH.data, nsim=3, modIn=mod.test,declineRate="-0.50",nyrs=10,nvisits=3,speciesName="SALS",scenario="FullPanel")

#########################################################################################
#test divideUnitsHalf

divideUnitsHalf(dataIn=region1.SMI.data)

#########################################################################################
#loop to get divide all SMI_Unit data in halves
SMI.data<-subset(data, is.na(SMI_Unit)==FALSE)

refugeList<-sort(unique(as.character(SMI.data$NWR_code)))

for(i in 1:length(refugeList)){

  new.data<-subset(SMI.data, NWR_code==refugeList[i])
  divideUnitsHalf(dataIn=new.data)

}
#########################################################################################
#look at new data
data.group1<-read.csv(paste(getwd(), "Data","SMI_data_2011-2015_group1.csv",sep="/"),header=TRUE)
data.group1.BMH<-subset(data.group1, NWR_code=="BMH")
unique(data.group1.BMH$new.unitNum)

data.group2<-read.csv(paste(getwd(), "Data","SMI_data_2011-2015_group2.csv",sep="/"),header=TRUE)
data.group2.BMH<-subset(data.group2, NWR_code=="BMH")
unique(data.group2.BMH$new.unitNum)


#set up loops through refuges and speceis for JAGS model

# pbar<- txtProgressBar(min=0, max=length(speciesList),width=7*length(speciesList), style=3)
#make new refugeList
refugeList<-sort(unique(data$NWR_code))
newRefugeList<-refugeList[refugeList != "BMH"]
newRefugeList<-refugeList[refugeList != "BMH" & refugeList != "CHN" & refugeList != "CLB" & refugeList != "CPY"]
newRefugeList<-factor(newRefugeList)

refugeList<-newRefugeList

refugeList<-"BMH"
speciesList<-"WILL"
declineList<-c("-0.50","-0.10")

#for running in parallel
detectCores()
cl<-makeCluster(8)
registerDoParallel(cl)

library(reshape)
library(plyr)
library(unmarked)
library(rjags)
library(R2WinBUGS)
library(arm)
library(ggplot2)
library(RColorBrewer)
library(maptools)
library(spdep)
library(pwr)
library(doParallel)
library(foreach)
library(dclone)

##################################################################################################################
##################################################################################################################


#set up paralellized cores for running JAGS

detectCores()
cl<-makeCluster(4)
registerDoParallel(cl)

SMI.data<-

#create declineList
declineList<-c("-0.10","-0.15","-0.20","-0.25","-0.30","-0.35","-0.40","-0.45","-0.50")
regionList<-sort(unique(as.character(data$regionList)))



for(z in 1:length(declineList)){
  #for(i in 1:length(refugeList)){
    foreach(i=1:length(refugeList),.packages=c("reshape","plyr","unmarked","rjags","arm","ggplot2","RColorBrewer")) %dopar% {
    for(j in 1:length(speciesList)){

    #read in refuge data
    message(paste("Getting data from ",refugeList[i], " for ",speciesList[j]," and running abundance models in 'unmarked'.",sep=""))

      if(speciesList[j]=="TMO"){
    refuge.data<-read.csv(paste(refugeDataFilepath,paste(refugeList[i],"data.tmo.csv",sep="."),sep="/"),header=TRUE)
      }else{
        refuge.data<-read.csv(paste(refugeDataFilepath,paste(refugeList[i],"data.csv",sep="."),sep="/"),header=TRUE)
      }
    #get overall unit estimates
    #run runUnmarked function
    mod.all<-tryCatch({runUnmarkedAllYears(dataIN=refuge.data, speciesName=speciesList[j])},
                    error=function(cond1){
                      cond1=NULL
                      message("Data did not meet minimal requirements for successful model fitting.")
                    })

    #run getModelOutput function
    message("Saving model results")
    try(getModelOutputAllYears(dataIN=refuge.data, modelIN=mod.all, speciesName=speciesList[j]))

    #run runUnmarked function
    mod.1<-tryCatch({runUnmarked(dataIN=refuge.data, speciesName=speciesList[j])},
                    error=function(cond1){
                      cond1=NULL
                      message("Data did not meet minimal requirements for successful model fitting.")
                    })

    #run getModelOutput function
    message("Saving model results")
    try(getModelOutput(dataIN=refuge.data, modelIN=mod.1, speciesName=speciesList[j]))


    #step 2: simulate data for given trends and run Bayesian power analysis in JAGS
    message("Simulating new data and running power analysis in JAGS.")
    try(runJAGSmodel(dataIn=refuge.data, nsim=100, modIn=mod.1,declineRate=declineList[z],nyrs=5,nvisits=3,speciesName=speciesList[j]))
    }
  }
}
##################################################################################################################
##################################################################################################################
##################################################################################################################
#code to get progress bars working
steps <- length(speciesList)*100
bar <- txtProgressBar (min=0, max=steps,width=50, style=3)
for (i in 1:steps)
{
  setTxtProgressBar (bar, i)
  Sys.sleep (.25)
}



#########################################################################################
#Power Analysis 3. BAYESIAN POWER ANALYSIS WITH SPATIAL AUTOCORRELATION among SMI units
#########################################################################################
#Write BUGS model with SPATIAL AUTOCORRELATION (car.normal in WinBUGS)

cat("
    model {

    # PRIOR DISTRIBUTIONS
    p0 ~ dunif(0,1)
    alpha0 <- logit(p0)
    beta0 ~ dunif(0, 2)

    for(n in 1:nVisits){
    alpha1[n] ~ dunif(0,0.01) #fixed effects of year on abun
    }

    for(h in 1:nYears){
    beta1[h] ~ dunif(0, 0.01) #fixed effects of year on abun
    }

    for(g in 1:nUnits){
    beta2[g] ~ dnorm(0, 0.01) #fixed effects of unit on abun
    gamma[g]~dunif(0,5)
    }

    #priors for CAR portion of model
    group.tau<-1/(group.sigma*group.sigma) #priors for neighborhood-based spatial grouping
    group.sigma~dunif(0,100)

    #conditional autoregressive (CAR) model component
    u.group[1:nUnits] ~ car.normal(adj[], weights[], num[], group.tau)

    for(j in 1:sumNumNeigh){
    weights[j]<-1
    }

    for(i in 1:M){ # Loop over sites
    # Conditional multinomial cell probabilities
    pi[i,1] <- p[i]
    pi[i,2] <- p[i]*(1-p[i])
    pi[i,3] <- p[i]*(1-p[i])*(1-p[i])
    pi[i,4] <- p[i]*(1-p[i])*(1-p[i])*(1-p[i])
    pi[i,5] <- p[i]*(1-p[i])*(1-p[i])*(1-p[i])*(1-p[i])
    pi0[i] <- 1 - (pi[i,1] + pi[i,2] + pi[i,3] + pi[i,4] + pi[i,5])
    pcap[i] <- 1 - pi0[i]
    for(j in 1:5){
    pic[i,j] <- pi[i,j] / pcap[i]
    }

    # logit-linear model for detection: with visit and observer effects
    logit(p[i]) <- alpha0 + alpha1[visit[i]]

    #Abundance model for Year 1
    log(lambda[i])<- beta0  + beta2[unit[i]] + u.group[unit[i]]

    y[i,1] ~ dbin(pcap[i], N[i,1])
    N[i,1] ~ dpois(lambda[i])

    #for years after year 1 where decline in pop occurs
    for (t in 2:nYears){
    y[i,t] ~ dbin(pcap[i], N[i,t])

    L[i,t]<-N[i, t-1]*gamma[unit[i]]

    N[i,t]~ dpois(L[i,t])

      }#t
    }#i

    }
    ",fill=TRUE, file="BUGS.model.car.txt")

#########################################################################################
#Run Simulations using WinBUGS and 'R2WinBUGS'
detectCores()
#to run on parallel cores first register cores
makeCluster(4)

#define list of refuges
refugeNotAdjacent<-c("CHN","CLB", "MCI", "MNY","MSH", "NGR", "PTI", "RHI","SBM", "STK","WAL")

#not all refuges have adjacency matrices
refugeListCAR<-setdiff(refugeList, refugeNotAdjacent)

#run for TMO and WILL
speciesList<-c("SALS")

refugeListCAR<-c("BMH")

for(i in 1:length(refugeListCAR)){
  for(j in 1:length(speciesList)){

    #read in refuge data
    message(paste("Getting data from ",refugeListCAR[i], " for ",speciesList[j]," and running abundance models in 'unmarked'.",sep=""))

    refuge.data<-read.csv(paste(refugeDataFilepath,paste(refugeListCAR[i],"data.csv",sep="."),sep="/"),header=TRUE)

    #step 1: get baseline abundance estimates from 'unmarked'
    #     try(mod.1<-getAbundance(dataIN=refuge.data, refugeName=refugeListCAR[i], speciesName=speciesList[j]))
    #     setTxtProgressBar (pbar, j)
    #     Sys.sleep (.01)

    #run runUnmarked function
    mod.1<-tryCatch({runUnmarked(dataIN=refuge.data, speciesName=speciesList[j])},
                    error=function(cond1){
                      cond1=NULL
                      message("Data did not meet minimal requirements for successful model fitting.")
                    })
    #run getModelOutput function
    message("Saving model results")
    try(getModelOutput(dataIN=refuge.data, modelIN=mod.1, speciesName=speciesList[j]))

    #create refuge adjacency matrix among units
    message("Creating adjacency matrix for refuge units.")
    try(getMapAdjacency(shapefileName="FWS_R5_SMIunits_20150811b", refugeName=refugeListCAR[i]))
    ###  I need to figure out what to do with refuges with only 1 unit or no adjacencies

    #step 2: simulate data for given trends and run Bayesian power analysis in JAGS
    message("Simulating new data and running power analysis in WinBUGS.")
    try(runWinBUGS_CAR(dataIn=refuge.data, nsim=100, modIn=mod.1, declineRate="-0.10",nyrs=5,nvisits=3,speciesName=speciesList[j]))
  