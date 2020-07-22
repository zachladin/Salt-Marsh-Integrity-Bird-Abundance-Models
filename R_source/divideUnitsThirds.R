#function to divide units up into two groups (using pseudorandom number generator)
divideUnitsThirds<-function(dataIn){

  #function to test if length of list is even or odd
  is.divisBy3 <- function(x) x %% 3 == 0

  #function to test if length of list is even or odd
  is.even <- function(x) x %% 2 == 0

  new.data<-dataIn

  #get list of units for each refuge

  #get refugeList
  refugeList<-sort(unique(new.data$NWR_code))

  #loop through to randomly get half of units per refuge (what about odd numbered?)
  group1.out<-list()
  group2.out<-list()
  group3.out<-list()
  for(i in 1:length(refugeList)){

    new.refuge<-subset(new.data, NWR_code==refugeList[i])
    head(new.refuge)

    refugeName<-unique(new.refuge$NWR_code)

    refuge.unitList<-sort(unique(new.refuge$SMI_Unit))

    #generate list of integers for each unit
    refuge.numList<-seq_along(refuge.unitList)

    new.df<-data.frame(SMI_Unit=refuge.unitList, new.unitNum=refuge.numList)

    new.refuge.merge<-merge(new.refuge, new.df, by="SMI_Unit")
    head(new.refuge.merge)

    #generate random vector for group1 (1st third)
    units.group1<-sample(refuge.numList,round(length(refuge.unitList)/3,1),replace=FALSE)

    #units remaining (other 2/3)
    units.remain<-setdiff(refuge.numList,units.group1)

    #generate random vector for group2 (2nd third)
    units.group2<-sample(units.remain,round(length(units.remain)/2,1),replace=FALSE)

    #group 2 (other 1/3)
    units.group3<-setdiff(units.remain,units.group2)

    if(is.even(i)==TRUE){
      #subset data group1
      refuge.group1<-subset(new.refuge.merge, new.unitNum %in% units.group1)
      unique(refuge.group1$SMI_Unit)

      #subset data group2
      refuge.group2<-subset(new.refuge.merge, new.unitNum %in% units.group2)
      unique(refuge.group2$SMI_Unit)

      #subset data group3
      refuge.group3<-subset(new.refuge.merge, new.unitNum %in% units.group3)
      unique(refuge.group3$SMI_Unit)
    }else{
        #subset data group1
        refuge.group1<-subset(new.refuge.merge, new.unitNum %in% units.group3)
        unique(refuge.group1$SMI_Unit)

        #subset data group2
        refuge.group2<-subset(new.refuge.merge, new.unitNum %in% units.group2)
        unique(refuge.group2$SMI_Unit)

        #subset data group3
        refuge.group3<-subset(new.refuge.merge, new.unitNum %in% units.group1)
        unique(refuge.group3$SMI_Unit)
      }

    message(paste("Dividing data within ",refugeName," into 3 goups.",sep=""))

    #compile results for both groups
    group1.out<-rbind(group1.out, refuge.group1)

    group2.out<-rbind(group2.out, refuge.group2)

    group3.out<-rbind(group3.out, refuge.group3)

  }

  #create folder to save data
  saveDataFilepath<-paste(getwd(),"Data","Data_Thirds",sep="/")
  dir.create(saveDataFilepath)

  #save results within data
  message("Recompiling and saving results.")
  write.csv(group1.out, file=paste(getwd(),"Data","Data_Thirds","SMI_data_2011-2015_third1.csv",sep="/"))
  write.csv(group2.out, file=paste(getwd(),"Data","Data_Thirds","SMI_data_2011-2015_third2.csv",sep="/"))
  write.csv(group3.out, file=paste(getwd(),"Data","Data_Thirds","SMI_data_2011-2015_third3.csv",sep="/"))


}

#End