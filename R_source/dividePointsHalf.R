#function to divide units up into two groups (using pseudorandom number generator)
dividePointsHalf<-function(dataIn){

  #function to test if length of list is even or odd
  is.even <- function(x) x %% 2 == 0

  # new data
  new.data.1<-dataIn

  #remove Hexagons with NA

  new.data<-new.data.1[is.na(new.data.1$Hexagon)==FALSE,]

  #get refugeList
  hexagonList<-sort(unique(new.data$Hexagon))

  #first subset data into hexagons with even and odd nums of points
  compileEvenOdds<-list()
  message("Sorting hexagons by those containing even or odd numbers of points")
  for(i in 1:length(hexagonList)){

    new.hexagon<-subset(new.data, Hexagon==hexagonList[i])
    head(new.hexagon)

    hexagonName<-unique(new.hexagon$Hexagon)

    hexagon.pointList<-sort(unique(as.character(new.hexagon$PointID)))

    #test if there are even or odd number of points
    evenTest<-is.even(length(hexagon.pointList))

    new.df.1<-data.frame(Hexagon=hexagonName, IsEven=evenTest)

    compileEvenOdds<-rbind(compileEvenOdds, new.df.1)
  }

  #merge compileEvenOdds with data
  new.data.merge<-merge(new.data, compileEvenOdds, by=c("Hexagon"))

  #split data into 2 groups (even and odd)
  temp.data.even<-new.data.merge[new.data.merge$IsEven==TRUE,]
  temp.data.odd<-new.data.merge[new.data.merge$IsEven==FALSE,]

  #loop through Hexagons with EVEN # pts to randomly get half of points per hexagon (what about odd numbered?)
  group1.even<-list()
  group2.even<-list()

  hexagonList.even<-sort(unique(as.character(temp.data.even$Hexagon)))

  for(i in 1:length(hexagonList.even)){

    new.hexagon.even<-subset(temp.data.even, Hexagon==hexagonList.even[i])
    head(new.hexagon.even)

    hexagonName<-unique(new.hexagon.even$Hexagon)

    hexagon.pointList.even<-sort(unique(new.hexagon.even$PointID))

    #generate list of integers for each point
    hexagon.numList<-seq_along(hexagon.pointList.even)

    new.df.even<-data.frame(PointID=hexagon.pointList.even, new.pointNum=hexagon.numList)

    new.hexagon.merge.even<-merge(new.hexagon.even, new.df.even, by="PointID")
    head(new.hexagon.merge.even)

    #generate random vector for group1 (1st half)
    points.group1.even<-sample(hexagon.numList,round(length(hexagon.pointList.even)/2,1),replace=FALSE)

    #group 2 (other half)
    points.group2.even<-setdiff(hexagon.numList,points.group1.even)

    #subset data group1
    hexagon.group1.even<-subset(new.hexagon.merge.even, new.pointNum %in% points.group1.even)

    #subset data group2
    hexagon.group2.even<-subset(new.hexagon.merge.even, new.pointNum %in% points.group2.even)

    message(paste("Dividing data within hexagon ",hexagonName," into 2 randomized groups.",sep=""))
    #compile results for both groups
    group1.even<-rbind(group1.even, hexagon.group1.even)

    group2.even<-rbind(group2.even, hexagon.group2.even)

  }


  #loop through Hexagons with ODD # pts to randomly get half of points per hexagon (what about odd numbered?)
  hexagonList.odd<-sort(unique(temp.data.odd$Hexagon))

  group1.odd<-list()
  group2.odd<-list()

  for(j in 1:length(hexagonList.odd)){

    new.hexagon.odd<-subset(temp.data.odd, Hexagon==hexagonList.odd[j])
    #head(new.hexagon)

    hexagonName<-unique(new.hexagon.odd$Hexagon)

    hexagon.pointList.odd<-sort(unique(new.hexagon.odd$PointID))

    #generate list of integers for each point
    hexagon.numList<-seq_along(hexagon.pointList.odd)

    new.df.odd<-data.frame(PointID=hexagon.pointList.odd, new.pointNum=hexagon.numList)

    new.hexagon.merge.odd<-merge(new.hexagon.odd, new.df.odd, by="PointID")
    #head(new.hexagon.merge.odd)

    #generate random vector for group1 (1st half)
    points.group1.odd<-sample(hexagon.numList,round(length(hexagon.pointList.odd)/2,1),replace=FALSE)

    #group 2 (other half)
    points.group2.odd<-setdiff(hexagon.numList,points.group1.odd)

    if(is.even(j)==TRUE){
      #subset data group1
      hexagon.group1.odd<-subset(new.hexagon.merge.odd, new.pointNum %in% points.group1.odd)

      #subset data group2
      hexagon.group2.odd<-subset(new.hexagon.merge.odd, new.pointNum %in% points.group2.odd)

    } else{
      #subset data group1
      hexagon.group1.odd<-subset(new.hexagon.merge.odd, new.pointNum %in% points.group2.odd)

      #subset data group2
      hexagon.group2.odd<-subset(new.hexagon.merge.odd, new.pointNum %in% points.group1.odd)
    }

    message(paste("Dividing data within hexagon ",hexagonName," into 2 randomized groups.",sep=""))
    #compile results for both groups
    group1.odd<-rbind(group1.odd, hexagon.group1.odd)

    group2.odd<-rbind(group2.odd, hexagon.group2.odd)
}

  #get difference between number of points in group1.odd and group2.odd
  nPoints.diff<-abs(length(unique(group1.odd$PointID))-length(unique(group2.odd$PointID)))

  group1.odd.points<-length(unique(group1.odd$PointID))
  group2.odd.points<-length(unique(group2.odd$PointID))

  message("Balancing out number points for hexagons with odd numbers of points.")
  #remove random point from larger set of data
  if(group1.odd.points > group2.odd.points){

    group1.odd$PointID.num<-as.integer(group1.odd$PointID)

    #generate random vector for group1.odd (1st half)
    point.samp<-sample(group1.odd$PointID.num, nPoints.diff, replace=FALSE)

    group1.odd<-group1.odd[group1.odd$PointID.num != point.samp,]
    group1.odd$PointID.num<-NULL

  }else{
    group2.odd$PointID.num<-as.integer(group2.odd$PointID)

    #generate random vector for group1.odd (1st half)
    point.samp<-sample(group2.odd$PointID.num,nPoints.diff,replace=FALSE)

    group2.odd<-group2.odd[group2.odd$PointID.num!=point.samp,]
    group2.odd$PointID.num<-NULL
  }

  #combine groups 1 and 2 from evens and odds
  message("Compiling all points for group 1 and group 2.")
  group1.out<-rbind(group1.even, group1.odd)
  group2.out<-rbind(group2.even, group2.odd)

  #create folder to save data
  saveDataFilepath<-paste(getwd(),"Data","Data_Half_hexagons",sep="/")
  dir.create(saveDataFilepath)

  message("Recompiling and saving results.")
  #save results within data
  write.csv(group1.out, file=paste(getwd(),"Data","Data_Half_hexagons","Group1_data_2011-2015_half1.csv",sep="/"),row.names=FALSE)
  write.csv(group2.out, file=paste(getwd(),"Data","Data_Half_hexagons","Group2_data_2011-2015_half2.csv",sep="/"), row.names=FALSE)


}

#End