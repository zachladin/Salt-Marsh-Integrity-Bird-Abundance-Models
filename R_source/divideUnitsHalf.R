#function to divide units up into two groups (using pseudorandom number generator)
divideUnitsHalf<-function(dataIn){

  #function to test if length of list is even or odd
  is.even <- function(x) x %% 2 == 0

  new.data<-dataIn

  #get list of units for each refuge

  #get refugeList
  refugeList<-sort(unique(new.data$NWR_code))

  #loop through to randomly get half of units per refuge (what about odd numbered?)
  group1.out<-list()
  group2.out<-list()
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

    #generate random vector for group1 (1st half)
    units.group1<-sample(refuge.numList,round(length(refuge.unitList)/2,1),replace=FALSE)

    #group 2 (other half)
    units.group2<-setdiff(refuge.numList,units.group1)

    if(is.even(i)==TRUE){
      #subset data group1
      refuge.group1<-subset(new.refuge.merge, new.unitNum %in% units.group1)

      #subset data group2
      refuge.group2<-subset(new.refuge.merge, new.unitNum %in% units.group2)
    }else{
      #subset data group1
      refuge.group1<-subset(new.refuge.merge, new.unitNum %in% units.group2)

      #subset data group2
      refuge.group2<-subset(new.refuge.merge, new.unitNum %in% units.group1)
    }

    message(paste("Dividing data within ",refugeName," into 2 goups.",sep=""))

    #compile results for both groups
    group1.out<-rbind(group1.out, refuge.group1)

    group2.out<-rbind(group2.out, refuge.group2)

  }

  message("Recompiling and saving results.")

  write.csv(group1.out, file=paste(getwd(),"Data","Data_Half","SMI.group1.compiled.csv",sep="/"),row.names=FALSE)
  write.csv(group2.out, file=paste(getwd(),"Data","Data_Half","SMI.group2.compiled.csv",sep="/"),row.names=FALSE)

  # #create folder to save data
  # saveDataFilepath<-paste(getwd(),"Data","Data_Half",sep="/")
  # dir.create(saveDataFilepath)
  # dir.create(paste(saveDataFilepath,refugeName, sep="/"))
  #
  # message("Recompiling and saving results.")
  # #save results within data
  # write.csv(group1.out, file=paste(getwd(),"Data","Data_Half",refugeName,paste(refugeName,"_SMI_data_2011-2015_half1.csv",sep=""),sep="/"),row.names=FALSE)
  # write.csv(group2.out, file=paste(getwd(),"Data","Data_Half",refugeName,paste(refugeName,"_SMI_data_2011-2015_half2.csv",sep=""),sep="/"), row.names=FALSE)
  #
  # }

    #compile group1 and save
  # save.group1<-list()
  # for(i in 1:length(refugeList)){
  #   new.group1<-read.csv(paste(getwd(),"Data","Data_Half",refugeList[i],paste(refugeList[i],"_SMI_data_2011-2015_half1.csv",sep=""),sep="/"),header=TRUE)
  #
  #   save.group1<-rbind(save.group1, new.group1)
  #
  # }
  # message("Compiling and saving all Refuge results.")
  # write.csv(save.group1, file=paste(getwd(),"Data","Data_Half","SMI.group1.compiled.csv",sep="/"),row.names=FALSE)
  #
  # save.group2<-list()
  # for(i in 1:length(refugeList)){
  #   new.group2<-read.csv(paste(getwd(),"Data","Data_Half",refugeList[i],paste(refugeList[i],"_SMI_data_2011-2015_half2.csv",sep=""),sep="/"),header=TRUE)
  #
  #   save.group2<-rbind(save.group2, new.group2)
  #
  # }
  # write.csv(save.group2, file=paste(getwd(),"Data","Data_Half","SMI.group2.compiled.csv",sep="/"),row.names=FALSE)


}

#End