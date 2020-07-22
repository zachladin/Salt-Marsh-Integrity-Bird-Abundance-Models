#function to divide units up into two groups (using pseudorandom number generator)
dividePointsThirds<-function(dataIn){

  #function to test if length of list is even or odd
  is.divisBy3 <- function(x) x %% 3 == 0

  #function to test if length of list is even or odd
  is.even <- function(x) x %% 2 == 0

  # new data
  new.data<-dataIn

  #get refugeList
  hexagonList<-sort(unique(new.data$Hexagon))

  #first subset data into hexagons with even and odd nums of points
  compileThirds<-list()
  message("Sorting hexagons by those containing numbers of points divisible by 3.")
  for(i in 1:length(hexagonList)){

    new.hexagon<-subset(new.data, Hexagon==hexagonList[i])
    head(new.hexagon)

    hexagonName<-unique(new.hexagon$Hexagon)

    hexagon.pointList<-sort(unique(as.character(new.hexagon$PointID)))

    #test if there are even or odd number of points
    thirdTest<-is.divisBy3(length(hexagon.pointList))

    new.df.1<-data.frame(Hexagon=hexagonName, IsThird=thirdTest)

    compileThirds<-rbind(compileThirds, new.df.1)
  }

  #merge compileEvenOdds with data
  new.data.merge<-merge(new.data, compileThirds, by=c("Hexagon"))

  #split data into 2 groups (even and odd)
  temp.data.thirds<-new.data.merge[new.data.merge$IsThird==TRUE,]
  temp.data.other<-new.data.merge[new.data.merge$IsThird==FALSE,]

  #loop through Hexagons with EVEN # pts to randomly get half of points per hexagon (what about odd numbered?)
  group1.thirds<-list()
  group2.thirds<-list()
  group3.thirds<-list()

  hexagonList.thirds<-sort(unique(as.character(temp.data.thirds$Hexagon)))

  for(i in 1:length(hexagonList.thirds)){

    new.hexagon.thirds<-subset(temp.data.thirds, Hexagon==hexagonList.thirds[i])
    head(new.hexagon.thirds)

    hexagonName<-unique(new.hexagon.thirds$Hexagon)

    hexagon.pointList.thirds<-sort(unique(new.hexagon.thirds$PointID))

    #generate list of integers for each point
    hexagon.numList.thirds<-seq_along(hexagon.pointList.thirds)

    new.df.thirds<-data.frame(PointID=hexagon.pointList.thirds, new.pointNum=hexagon.numList.thirds)

    new.hexagon.merge.thirds<-merge(new.hexagon.thirds, new.df.thirds, by="PointID")
    head(new.hexagon.merge.thirds)

    #generate random vector for group1 (1st half)
    points.group1.thirds<-sample(hexagon.numList.thirds,round(length(hexagon.pointList.thirds)/3,1),replace=FALSE)

    #group 2 (temp)
    points.group2.thirds.temp<-setdiff(hexagon.numList.thirds, points.group1.thirds)

    #group 3 (remaining)
    points.group3.thirds<-sample(points.group2.thirds.temp,round(length(points.group2.thirds.temp)/2,1),replace=FALSE)

    #group 2 (real)
    points.group2.thirds<-setdiff(points.group2.thirds.temp, points.group3.thirds)


    #subset data group1
    hexagon.group1.thirds<-subset(new.hexagon.merge.thirds, new.pointNum %in% points.group1.thirds)

    #subset data group2
    hexagon.group2.thirds<-subset(new.hexagon.merge.thirds, new.pointNum %in% points.group2.thirds)

    #subset data group2
    hexagon.group3.thirds<-subset(new.hexagon.merge.thirds, new.pointNum %in% points.group3.thirds)


    message(paste("Dividing data within hexagon ",hexagonName," into 3 randomized groups.",sep=""))
    #compile results for both groups
    group1.thirds<-rbind(group1.thirds, hexagon.group1.thirds)

    group2.thirds<-rbind(group2.thirds, hexagon.group2.thirds)

    group3.thirds<-rbind(group3.thirds, hexagon.group3.thirds)

  }

  length(unique(group3.thirds$PointID))

  #loop through Hexagons with ODD # pts to randomly get half of points per hexagon (what about odd numbered?)
  hexagonList.other<-sort(unique(temp.data.other$Hexagon))

  ##########################################################################################
  #get table of numPoints within each of remaining hexagons
  numPoints.table<-list()
  for(z in 1:length(hexagonList.other)){
    new.hexagon.other<-subset(temp.data.other, Hexagon==hexagonList.other[z])
    hexagonName<-unique(new.hexagon.other$Hexagon)

    numPoints<-length(sort(unique(new.hexagon.other$PointID)))

    new.df.other<-data.frame(Hexagon=hexagonName, numPoints=numPoints)
    numPoints.table<-rbind(numPoints.table, new.df.other)

  }
  #get max points within a Hexagon
  max(numPoints.table$numPoints)

  #sort by
  numPoints.table.sort<-numPoints.table[order(numPoints.table$numPoints,decreasing = TRUE),]

  #grab new hexagonList
  newHexagonList<-c(numPoints.table.sort$Hexagon)
  ##########################################################################################

  group1.other<-list()
  group2.other<-list()
  group3.other<-list()

  group1.length<-0
  group2.length<-0
  group3.length<-0
  for(j in 1:length(newHexagonList)){

    new.hexagon.other<-subset(temp.data.other, Hexagon==newHexagonList[j])
    new.hexagon.other$PointID<-as.factor(as.character(new.hexagon.other$PointID))

    new.hexagon.other$new.pointNum<-as.integer(new.hexagon.other$PointID)

    pointList<-sample(unique(new.hexagon.other$new.pointNum))

    pointList.thirds<-split(pointList, ceiling(seq_along(pointList)/(length(pointList)/3)))

      group1.points<-unname(unlist(pointList.thirds[1]))
      group2.points<-unname(unlist(pointList.thirds[2]))
      group3.points<-unname(unlist(pointList.thirds[3]))

      #get lengths
      group1.length<-length(group1.points)
      group2.length<-length(group2.points)
      group3.length<-length(group3.points)

    #subset group1.other
    group1.sub<-subset(new.hexagon.other, new.pointNum %in% group1.points)

    group2.sub<-subset(new.hexagon.other, new.pointNum %in% group2.points)

    group3.sub<-subset(new.hexagon.other, new.pointNum %in% group3.points)

    sum.points<-sum(group1.length, group2.length, group3.length)

    #rbind to store results
    if((sum.points==1)==TRUE & is.divisBy3(j)==FALSE){
      group1.other<-rbind(group1.other, group1.sub)
      group2.other<-rbind(group2.other, group3.sub)
      group3.other<-rbind(group3.other, group2.sub)
    }else{
      if((sum.points==1)==TRUE & is.divisBy3(j)==TRUE){
        group1.other<-rbind(group1.other, group3.sub)
        group2.other<-rbind(group2.other, group1.sub)
        group3.other<-rbind(group3.other, group2.sub)
      }else{
        if((sum.points<5)==TRUE & is.divisBy3(j)==TRUE){
        group1.other<-rbind(group1.other, group1.sub)
        group2.other<-rbind(group2.other, group3.sub)
        group3.other<-rbind(group3.other, group2.sub)
        }else{
          if((sum.points<5)==TRUE & is.divisBy3(j)==FALSE){
          group1.other<-rbind(group1.other, group3.sub)
          group2.other<-rbind(group2.other, group1.sub)#flip 2 and 1
          group3.other<-rbind(group3.other, group2.sub)
        }else{
          if((is.even(sum.points))==TRUE & is.even(j)==FALSE){
          group1.other<-rbind(group1.other, group1.sub)
          group2.other<-rbind(group2.other, group2.sub)
          group3.other<-rbind(group3.other, group3.sub)
          }else{
            if((is.even(sum.points))==TRUE & is.even(j)==TRUE){
              group1.other<-rbind(group1.other, group1.sub)
              group2.other<-rbind(group2.other, group3.sub)
              group3.other<-rbind(group3.other, group2.sub)
            }else{
              if((is.even(sum.points))==FALSE & is.divisBy3(j)==TRUE){
              group1.other<-rbind(group1.other, group3.sub)
              group2.other<-rbind(group2.other, group2.sub)
              group3.other<-rbind(group3.other, group1.sub)
            }else{
              if((is.even(sum.points))==FALSE & is.divisBy3(j)==FALSE){
                group1.other<-rbind(group1.other, group3.sub)#3, 1, 2
                group2.other<-rbind(group2.other, group1.sub)
                group3.other<-rbind(group3.other, group2.sub)
              }}}}}}}}

  }

  length(unique(group1.other$PointID))
  length(unique(group2.other$PointID))
  length(unique(group3.other$PointID))

  #combine groups 1 and 2 from evens and others
  message("Compiling all points for group 1 and group 2.")
  group1.out<-rbind(group1.thirds, group1.other)
  group2.out<-rbind(group2.thirds, group2.other)
  group3.out<-rbind(group3.thirds, group3.other)

  message("Balancing out number points for each of the datasets.")

  #move 2 randomly selected points from group3 to group2
  group3.out$new.pointNum<-as.integer(group3.out$PointID)
  #choose 2 random points
  group3.numList<-sample(group3.out$new.pointNum, 2, replace=FALSE)
  #subset out from group3
  group3.2rand<-subset(group3.out, c(new.pointNum==group3.numList[1] | new.pointNum==group3.numList[2]))
  #add randomly selected points to group2
  group2.out.new<-rbind(group2.out,group3.2rand)

  #remove randomly selected points from group3
  group3.out.new<-subset(group3.out,c(new.pointNum !=1145 & new.pointNum !=1873))

  #check to make sure numbers are balanced
  length(unique(group1.out$PointID))
  length(unique(group2.out.new$PointID))
  length(unique(group3.out.new$PointID))


  #create folder to save data
  saveDataFilepath<-paste(getwd(),"Data","Data_Thirds_hexagon",sep="/")
  dir.create(saveDataFilepath)

  message("Recompiling and saving results.")
  #save results within data
  write.csv(group1.out, file=paste(getwd(),"Data","Data_Thirds_hexagon","Group1_data_2011-2015.csv",sep="/"),row.names=FALSE)
  write.csv(group2.out.new, file=paste(getwd(),"Data","Data_Thirds_hexagon","Group2_data_2011-2015.csv",sep="/"), row.names=FALSE)
  write.csv(group3.out.new, file=paste(getwd(),"Data","Data_Thirds_hexagon","Group3_data_2011-2015.csv",sep="/"), row.names=FALSE)


}

#End