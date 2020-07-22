#########################################################################################
#SMI data analysis and visualizaiton - Ladin et al. 2016

#########################################################################################
#clear environment
rm(list=ls())

getwd()

###########################################################################################
#set working directory (put your filepath inside quotes here)
setwd("/Users/Zach/Dropbox (ZachTeam)/Projects/smi_projects/SMI_bird_analyses_working")

#on Greg's machine
#setwd("/Users/Greg/Dropbox/ZachGreg/SHARP_analyses")

###########################################################################################
#now compare gmultmix results with multPois results

multiPois.results<-read.csv(paste(getwd(), "Results","Summary_results","MPois_Summary.table.out_4-07-2020.csv",sep="/"))
multiPois.results$X<-NULL

#add Model_Type column
multiPois.results$Model_Type<-"Multinomial_Poisson"

gmmult.results<-read.csv(paste(getwd(), "Results","Summary_results","Summary.table.out.csv",sep="/"))

gmmult.results$Model_Type<-"Gmultmix"

#stack data
results.data<-rbind(multiPois.results, gmmult.results)
results.data<-subset(results.data, ! is.na(SMI_Unit))
results.data$SMI_Unit<-factor(results.data$SMI_Unit, levels=rev(levels(results.data$SMI_Unit)))
results.data$AlphaCode<-factor(results.data$AlphaCode, levels=c("TMO","WILL","SALS","SESP","CLRA","NESP"))
#now plot comparison
library(ggplot2)

mycolors<-c("royalblue4","tomato")

AbunPlot<-ggplot(data=subset(results.data, AlphaCode !="NESP"), aes(x=SMI_Unit, y=Predicted))+
  # geom_boxplot(aes(fill=Model_Type), size=0.1, position=position_dodge(width=0.9))+
  geom_errorbar(aes(ymin=Predicted-SE, ymax=Predicted+SE, color=Model_Type), size=0.3,width=0,position=position_dodge(width=0.8))+
  geom_point(aes(color=Model_Type), size=0.2,position=position_dodge(width=0.8))+
  scale_fill_manual(values=mycolors)+
  scale_color_manual(values=mycolors)+
  coord_flip()+
  #ylim(c(-2.5,10))+
  scale_y_continuous(expand=c(0,0),limits = c(-5,20), breaks=c(-5, 0, 5, 10, 15, 20))+
  labs(y="Estimated Abundance")+
  theme(panel.border = element_rect(color="black",fill="transparent"),
        axis.text.y=element_text(size=7),
        axis.text.x=element_text(size=7),
        legend.position = "top")

AbunPlotFacet<-AbunPlot+facet_grid(~AlphaCode)
AbunPlotFacet

#improve this plot, and save to include with response to reviewers


ggsave(AbunPlotFacet, filename="/Users/zach/Dropbox (ZachTeam)/Manuscripts/Marsh Bird Monitoring Design/Bird_SMI_power_ms/Submission_Global_Ecology_and_Conservation/ModelComparison_4-08-2020.png", width=10, height=11, dpi=300)













###########################################################################################
#if needed install required packages
install.packages(c("reshape","plyr","unmarked","rjags", "arm", "ggplot2",
                   "RColorBrewer","doParallel",
                   "foreach","dclone","data.table"))

#########################################################################################
#load 'loadPackages.R' source file
source(paste(getwd(),"R_source","loadPackages.R",sep="/"))

#########################################################################################
#use loadPackages function to load remaining functions and packages
loadPackages()

#########################################################################################
#read in combined data
dataRaw<-read.csv(paste(getwd(),"Data","SMI_data","dataRawCombined.csv",sep="/"),header=TRUE)
length(unique(dataRaw$unit_ID))

###########################################################################################################
#format raw data
formatRawData(dataIn=dataRaw, saveFileName="SMI.formatted.data.new.csv")

###########################################################################################################
#read in formatted data
data<-read.csv(paste(getwd(),"Data","SMI_data","SMI.formatted.data.new.csv",sep="/"))

unitList<-sort(as.character(unique(data$SMI_Unit)))

refugeList<-sort(as.character(unique(data$NWR_code)))

###########################################################################################################
#test get abundance by hexagon function

data$Hexagon<-as.character(data$Hexagon)
data$AlphaCode<-as.character(data$AlphaCode)

#first getHexagon data
getHexagonData(dataIn=data)


#hexagonList
hexagonList<-as.character(unique(data$Hexagon))

#speciesList
speciesList<-c("SALS","SESP","CLRA","WILL","NESP","TMO")


species.save<-list()
for(j in 1:length(speciesList)){ 

  species<-as.character(speciesList[j])

hexagon.save<-list()
for(i in 1:length(hexagonList)){
  
  new.data<-subset(data, Hexagon==hexagonList[i])
  abun<-NULL
  abun<-getAbundanceHexagon(dataIn=new.data, speciesIn=species)
  abun$Species<-species
  
  hexagon.save<-rbind(hexagon.save, abun)
  
  }
  species.save<-rbind(species.save, hexagon.save)
}

write.csv(species.save, file=paste(getwd(), "Results","Hexagon","All_hexagon_abundances.csv",sep="/"),row.names=FALSE)
  

###########################################################################################################

#filter by SMI years
SMIyears<-read.csv(paste(getwd(),"Data","SMI_data","SMIUnits_Year_table.csv",sep="/"),header=TRUE)
SMIyears<-SMIyears[,c("Refuge","SMI_Unit","Year")]
SMIyears$unit.year<-paste(SMIyears$SMI_Unit, SMIyears$Year, sep="_")

###########################################################################################################
#add unit.year column
data$unit.year<-paste(data$SMI_Unit, data$Year, sep="_")

###########################################################################################################
#now merge data (to filter out years)
data.merge<-merge(SMIyears,data, by="unit.year",all.x=TRUE)

#remove Year.y column
data.merge$Year.y<-NULL
colnames(data.merge)[4]<-"Year"

data.merge$SMI_Unit.y<-NULL
colnames(data.merge)[3]<-"SMI_Unit"

#save as true SMI data (1 year each)
write.csv(data.merge, file=paste(getwd(), "Data","SMI_Data","SMI_true_data.csv"),row.names=FALSE)
#########################################################################################
#########################################################################################
#Start Here:
#########################################################################################
#read in new data
data<-read.csv(paste(getwd(), "Data","SMI_Data","SMI_true_data.csv",sep="/"),header=TRUE)

#########################################################################################
#subset and save data (Full dataset) by NWR
getRefugeData(dataIn=data)


#################################################################################################
#get list of SMI units

unitList<-sort(unique(data$SMI_Unit))

#how many units in each refuge

#get list of NWRs in order from N to S
refugeList.order.data<-read.csv(paste(getwd(), "Data","SMI_data","NWR_Order.csv",sep="/"),header=TRUE)
refugeList.order<-as.character(refugeList.order.data[,c("NWR_code")])

refugeList<-refugeList.order


table.out<-list()
for(i in 1:length(refugeList)){
  new.data<-subset(data, NWR_code==refugeList[i])
  refugeName<-unique(new.data$NWR_code)
  numUnits<-length(unique(new.data$SMI_Unit))

  new.df<-data.frame(NWR=refugeName, numUnits=numUnits)
  table.out<-rbind(table.out, new.df)

}
table.out

#########################################################################################
#get list of NWRs in order from N to S
refugeList.order.data<-read.csv(paste(getwd(), "Data","SMI_data","NWR_Order.csv",sep="/"),header=TRUE)
refugeList.order<-as.character(refugeList.order.data[,c("NWR_code")])

#refugeList<-sort(as.character(unique(data$NWR_code)))

speciesList=c("SALS","SESP","NESP","CLRA","WILL","TMO")
#speciesList=c("TMO")


#########################################################################################
#If you want to run parallelized loops:
detectCores()
cl<-makeCluster(15)
registerDoParallel(cl)

#########################################################################################
source("/Users/zach/Dropbox (ZachTeam)/Projects/smi_projects/SMI_bird_analyses_working/R_source/makeMultPoisumf.R")
source("/Users/zach/Dropbox (ZachTeam)/Projects/smi_projects/SMI_bird_analyses_working/R_source/runUnmarkedAllYears_multiPois.R")
source("/Users/zach/Dropbox (ZachTeam)/Projects/smi_projects/SMI_bird_analyses_working/R_source/getModelOutputAllYears_multiPois.R")
source("/Users/zach/Dropbox (ZachTeam)/Projects/smi_projects/SMI_bird_analyses_working/R_source/getAbundanceSMI_multiPois.R")


#Just loop through abundance estimates for each SMI unit
refuge.table.out<-list()
species.table.out<-list()
for(i in 1:length(refugeList)){

  refugeName<-as.character(refugeList[i])

  for(j in 1:length(speciesList)){
  #foreach(j=1:length(speciesList),.packages=c("reshape","plyr","unmarked","rjags","arm","ggplot2","RColorBrewer","data.table","doParallel","foreach","dclone")) %dopar% {

    speciesName<-as.character(speciesList[j])
    print(speciesName)
    
    message(paste("Getting data from ",refugeName, " for ",speciesName," and running abundance models in 'unmarked'.",sep=""))
    #read in refuge data
    if(speciesName=="TMO"){
      refuge.data<-read.csv(paste(getwd(),"Data","Refuge_data",refugeName,paste(refugeName,"data.tmo.csv",sep="."),sep="/"),header=TRUE)
    }else{
      refuge.data<-read.csv(paste(getwd(),"Data","Refuge_data",refugeName,paste(refugeName,"data.csv",sep="."),sep="/"),header=TRUE)
    }

    species.table<-try(getAbundanceSMI_multiPois(dataIn=refuge.data, speciesName=speciesList[j]))

    species.table.2<-if(is.null(species.table)==TRUE){
                    data.frame(SMI_Unit=NA, Predicted=NA, SE=NA, lower=NA, upper=NA, Longitude=NA, Latitude=NA, model.formula=NA)
                        }else{species.table}

    #this doesn't work with foreach :(
    species.table.2$NWR_code<-refugeName
    species.table.2$AlphaCode<-speciesName

    species.table.out<-rbind(species.table.out, species.table.2)
  }
  refuge.table.out<-rbind(refuge.table.out,species.table.out)
}

dir.create(paste(getwd(), "Results","Summary_results",sep="/"))
write.csv(refuge.table.out, file=paste(getwd(), "Results","Summary_results","MPois_Summary.table.out_4-07-2020.csv",sep="/"))

########################################################################
#compiler loops

refugeList<-sort(as.character(unique(data$NWR_code)))


speciesList=c("SALS","SESP","NESP","CLRA","WILL","TMO")


#Just loop through abundance estimates for each SMI unit
refuge.table.out<-list()
species.table.out<-list()
for(i in 1:length(refugeList)){

  refugeName<-as.character(refugeList[i])

  for(j in 1:length(speciesList)){

    speciesName<-as.character(speciesList[j])


      species.table<-tryCatch({read.csv(paste(getwd(),"Results",speciesName,refugeName,
                                    paste(speciesName, refugeName, "abundance.by.unit.csv",sep="."),sep="/"),header=TRUE)
                       },error=function(cond2){
                        cond2=NULL
                        cond2
                        })

      species.table.2<-if(is.null(species.table)==TRUE){
                   data.frame(SMI_Unit=NA, Predicted=NA, SE=NA, lower=NA, upper=NA, Longitude=NA, Latitude=NA, model.formula=NA)
                    }else{species.table}

      #add NWR_code and AlphaCode columns
      species.table.2$NWR_code<-refugeName
      species.table.2$AlphaCode<-speciesName

       species.table.out<-rbind(species.table.out, species.table.2)
    }
  refuge.table.out<-rbind(refuge.table.out,species.table.out)
}

dir.create(paste(getwd(), "Results","Summary_results",sep="/"))
write.csv(refuge.table.out, file=paste(getwd(), "Results","Summary_results","Summary.table.out.csv",sep="/"),row.names=FALSE)

##################################################################################################
#make some cool plots
abun<-read.csv(paste(getwd(), "Results","Summary_results","Summary.table.out.csv",sep="/"),header=TRUE)

abun.1<-unique(abun)
#remove SMI_Unit = NA
#abun<-abun[is.na(abun$SMI_Unit)==FALSE,]

#get dataframe of all units and species (to show non-detections)
unit.df<-unique(data[,c("NWR_code","SMI_Unit","Longitude","Latitude")])

unit.df.species.out<-list()
for(i in 1:length(speciesList)){
  new.data<-unit.df
  speciesName<-as.character(speciesList[i])
  new.data.2<-data.frame(AlphaCode=speciesName,new.data)

  unit.df.species.out<-rbind(unit.df.species.out, new.data.2)
}

#now merge with list of all SMI units
species.merge<-merge(unit.df.species.out, abun.1,by=c("SMI_Unit","AlphaCode"),all.x=TRUE)

species.merge$Predicted[is.na(species.merge$Predicted)]<-0
species.merge$SE[is.na(species.merge$SE)]<-0
species.merge$NWR_code.y<-NULL
species.merge$Latitude.y<-NULL
species.merge$Longitude.y<-NULL

colnames(species.merge)[3:5]<-c("NWR_code","Longitude","Latitude")

#reorder factor NWR_code

#sort species abundance by SMI_Unit
species.merge.1<-species.merge[order(species.merge$Latitude,decreasing=TRUE),]

species.merge.1$NWR_code<-factor(species.merge.1$NWR_code, levels=c(refugeList.order))

length(unique(species.merge.1$SMI_Unit))

#reorder factor levels for SMI_Unit
species.merge.1$SMI_Unit<-factor(species.merge.1$SMI_Unit, levels=rev(species.merge.1$SMI_Unit))

#reorder factor species
species.merge.1$AlphaCode<-factor(species.merge.1$AlphaCode, levels=c("SALS","SESP","NESP","CLRA","WILL","TMO"))

length(unique(species.merge.1$SMI_Unit))


head(abun)

#plot abundance estimates by year and unit
plot.abun<-ggplot(data=species.merge.1, aes(x=SMI_Unit, y=Predicted, ymin=Predicted, ymax=Predicted+SE))+
  coord_flip()+
  geom_errorbar(width=0, size=0.25, position=position_dodge(width=0.9),aes(color=NWR_code))+
  #geom_point(stat="identity",size=0.7,aes(color=NWR_code),alpha=1)+
  geom_bar(stat="identity",position=position_dodge(),aes(fill=NWR_code),alpha=0.7)+
  #ggtitle(paste(speciesName," abundance in ", refugeName, " by SMI Unit"))+
  #theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), panel.border=element_blank())+
  #theme(panel.background=element_rect(fill='white'))+
  theme(axis.text.x = element_text(size=8, color="black"),
        axis.text.y = element_text(size=6, color="black"),
        axis.title.x = element_text(size=15, hjust=0.5, vjust=0.2),
        axis.title.y = element_text(angle = 90, vjust=1.2, size=15))+
  theme(panel.border=element_rect(color="black",fill=NA))+
  #theme(legend.position="none")+
  lims(y=c(0,10))+
  #scale_fill_brewer(palette="Set2")+
  #scale_color_brewer(palette="Set2")+
  labs(x="SMI Unit", y="Estimated mean abundance per point")
#scale_y_continuous(expand = c(0,0), limits = c(0, max(species.abun$Predicted+species.abun$SE)+0.5))
# guides(fill = guide_legend(reverse = TRUE), color=guide_legend(reverse = TRUE))
#abun.plot.out<-plot.abun+facet_wrap(~Year,ncol=plot.years)
plot.abun


abun.plot.facet<-plot.abun+facet_grid(.~AlphaCode)
abun.plot.facet

# #Now by refuge as well
# abun.plot.facet.nwr<-plot.abun+facet_wrap(NWR_code~AlphaCode,scales="free")
# abun.plot.facet.nwr


#save plot of abundance estimates
myFilepath<-paste(getwd(), "Results","Summary_results",sep="/")
ggsave(abun.plot.facet, filename="SMI.all.focal.species.summary.abun.fig.pdf",path=myFilepath, width=10,height=8.5, limitsize=FALSE)
ggsave(abun.plot.facet, filename="SMI.all.focal.species.summary.abun.fig.png",path=myFilepath, width=10,height=8.5, limitsize=FALSE)


########

#get list of NWRs in order from N to S
refugeList.order.data<-read.csv(paste(getwd(), "Data","SMI_data","NWR_Order.csv",sep="/"),header=TRUE)
refugeList.order<-as.character(refugeList.order.data[,c("NWR_code")])

#for loop to get each species fig separately.

for(i in 1:length(speciesList)){

  species.data.1<-subset(species.merge.1, AlphaCode==speciesList[i])

  # #reorder factor levels for NWR_code
  species.data.1$NWR_code<-factor(species.data.1$NWR_code,levels= refugeList.order)


  #sort species abundance by NWR then by Latitude
  species.data.1<-species.data.1[order(species.data.1$NWR_code,-species.data.1$Latitude),]


  #reorder factor levels for SMI_Unit
  species.data.1$SMI_Unit<-factor(species.data.1$SMI_Unit, levels=rev(species.data.1$SMI_Unit))


  speciesName<-as.character(unique(species.data.1$AlphaCode))

  #plot abundance estimates by year and unit
  plot.abun<-ggplot(data=species.data.1, aes(x=SMI_Unit, y=Predicted, ymin=Predicted, ymax=Predicted+SE))+
    coord_flip()+
    geom_errorbar(width=0, size=0.25, position=position_dodge(width=0.9),aes(color=NWR_code))+
    #geom_point(stat="identity",size=0.7,aes(color=NWR_code),alpha=1)+
    geom_bar(stat="identity",position=position_dodge(),aes(fill=NWR_code),size=0.1,alpha=0.7)+
    ggtitle(paste(speciesName," abundance by SMI unit"))+
    #theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), panel.border=element_blank())+
    theme(panel.background=element_rect(fill='white'))+
    theme(axis.text.x = element_text(size=8, color="black"),
          axis.text.y = element_text(size=6, color="black"),
          axis.title.x = element_text(size=15, hjust=0.5, vjust=0.2),
          axis.title.y = element_text(angle = 90, vjust=1.2, size=15))+
    theme(panel.border=element_rect(color="black",fill=NA))+
    theme(legend.position="none")+
    lims(y=c(0,10))+
    #scale_fill_brewer(palette="Set2")+
    #scale_color_brewer(palette="Set2")+
    labs(x="SMI Unit", y="Estimated mean abundance per point")
  #scale_y_continuous(expand = c(0,0), limits = c(0, max(species.abun$Predicted+species.abun$SE)+0.5))
  # guides(fill = guide_legend(reverse = TRUE), color=guide_legend(reverse = TRUE))
  #abun.plot.out<-plot.abun+facet_wrap(~Year,ncol=plot.years)
  plot.abun

  print(plot.abun)


  #save plot of abundance estimates
  myFilepath<-paste(getwd(), "Results","Summary_results",sep="/")
  ggsave(plot.abun, filename=paste(speciesName,"SMI.summary.abun.fig.pdf",sep="."),path=myFilepath, width=5,height=8, limitsize=FALSE)
  ggsave(plot.abun, filename=paste(speciesName,"SMI.summary.abun.fig.png",sep="."),path=myFilepath, width=5,height=8, limitsize=FALSE)

}



##################################################################################
#try making a map
library(ggmap)


#make some cool plots
abun<-read.csv(paste(getwd(), "Results","Summary_results","Summary.table.out.csv",sep="/"),header=TRUE)

#remove SMI_Unit = NA
abun<-abun[is.na(abun$SMI_Unit)==FALSE,]

#sort species abundance by SMI_Unit
abun<-abun[order(abun$Latitude,decreasing=TRUE),]

#reorder factor levels for SMI_Unit
abun$SMI_Unit<-factor(abun$SMI_Unit, levels=rev(abun$SMI_Unit))

#reorder factor species
abun$AlphaCode<-factor(abun$AlphaCode, levels=c("SALS","SESP","NESP","CLRA","WILL","TMO"))

unit.order<-read.csv(paste(getwd(), "Data","SMI_data","NWR_Order.csv",sep="/"))
head(unit.order)

refugeList.order<-as.character(unit.order[,c("NWR_code")])

#reorder factor NWR_code
abun$NWR_code<-factor(abun$NWR_code, levels=c(refugeList.order))

#remove outlying abundance estimates
abun.lim<-abun[abun$Predicted<20,]

#remove crazy SEs
abun.lim.2<-abun.lim[abun.lim$SE<20,]

#remove NAs
abun<-abun.lim.2[is.na(abun.lim.2$AlphaCode)==FALSE,]

maxSize<-max(abun$Predicted,na.rm=TRUE)
minSize<-min(abun$Predicted,na.rm=TRUE)

#get extent
minLon=min(abun$Longitude,na.rm=TRUE)-1
maxLon=max(abun$Longitude,na.rm=TRUE)+1
minLat=min(abun$Latitude,na.rm=TRUE)-1
maxLat=max(abun$Latitude,na.rm=TRUE)+1

bbox.1<-c(left=minLon, bottom=minLat,right=maxLon,top=maxLat)

#get base map (requires internet connection)
map.1 <- get_map(location = bbox.1,maptype="toner-lite")
ggmap(map.1)

map.all<-ggmap(map.1, extent = "panel", maprange=TRUE,alpha=0.8)+
  geom_point(data=abun,stat="identity",aes(x=Longitude, y=Latitude, color=NWR_code,size=Predicted+SE),alpha=0.05)+
  geom_point(data=abun,stat="identity",aes(x=Longitude, y=Latitude, color=NWR_code,size=Predicted),alpha=1)+

  #scale_color_gradient(low="red",high="royalblue4",limits=c(-20,20))+
  #scale_color_manual(values=mapColors,guide=guide_legend(order=1))+
  guides(alpha=FALSE)+
  labs(x="Longitude",y="Latitude")+
  #ggtitle(paste("SET stations (n = ",nStations,")",sep=""))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  scale_radius(guide=guide_legend(title="Abundance",order=1),range=c(1,6))
map.all

map.facet<-map.all+facet_wrap(~AlphaCode,nrow=2)
map.facet
print(map.all)

#create folder to save results
dir.create(paste(getwd(), "Results","Summary_results","Maps", sep="/"))

#save Map
ggsave(map.facet, filename="SMI_bird_abundance_map_facet.png",path=paste(getwd(),"Results","Summary_results","Maps",sep="/"), width=9,height=6.5, limitsize=FALSE)
ggsave(map.facet, filename="SMI_bird_abundance_map_facet.pdf",path=paste(getwd(),"Results","Summary_results","Maps",sep="/"), width=9,height=6.5, limitsize=FALSE)
#########################################################

for(i in 1:length(speciesList)){

  species.data<-subset(abun, AlphaCode==speciesList[i])
  speciesName<-as.character(unique(species.data$AlphaCode))

  map.all<-ggmap(map.1, extent = "panel", maprange=TRUE,alpha=0.8)+
    geom_point(data=species.data,stat="identity",aes(x=Longitude, y=Latitude, color=NWR_code,size=Predicted+SE),alpha=0.05)+
    geom_point(data=species.data,stat="identity",aes(x=Longitude, y=Latitude, color=NWR_code,size=Predicted),alpha=0.8)+
    #scale_color_gradient(low="red",high="royalblue4",limits=c(-20,20))+
    #scale_color_manual(values=mapColors,guide=guide_legend(order=1))+
    guides(alpha=FALSE)+
    labs(x="Longitude",y="Latitude")+
    ggtitle(paste("Estimated abundance per point for ",speciesName,sep=""))+
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
    scale_radius(guide=guide_legend(title="Abundance",order=1),range=c(0.5,7))
  map.all

  print(map.all)

  #create folder to save results
  dir.create(paste(getwd(), "Results","Summary_results","Maps", sep="/"))

  #save Map
  ggsave(map.all, filename=paste(speciesName,"abundance_map.png",sep="."),path=paste(getwd(),"Results","Summary_results","Maps",sep="/"), width=6,height=7, limitsize=FALSE)
  ggsave(map.all, filename=paste(speciesName,"abundance_map.pdf",sep="."),path=paste(getwd(),"Results","Summary_results","Maps",sep="/"), width=6,height=7, limitsize=FALSE)


}
###################################################################
#make each map separately

#SALS
species.data<-subset(abun, AlphaCode==speciesList[1])
speciesName<-as.character(unique(species.data$AlphaCode))

map.sals<-ggmap(map.1, extent = "panel", maprange=TRUE,alpha=0.8)+
  geom_point(data=species.data,stat="identity",aes(x=Longitude, y=Latitude, color=NWR_code,size=Predicted+SE),alpha=0.05)+
  geom_point(data=species.data,stat="identity",aes(x=Longitude, y=Latitude, color=NWR_code,size=Predicted),alpha=0.8)+
  #scale_color_gradient(low="red",high="royalblue4",limits=c(-20,20))+
  #scale_color_manual(values=mapColors,guide=guide_legend(order=1))+
  guides(alpha=FALSE)+
  labs(x="Longitude",y="Latitude")+
  ggtitle(paste("Estimated abundance per point for ",speciesName,sep=""))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  scale_radius(guide=guide_legend(title="Abundance",order=1),range=c(0.5,7))

#SESP
species.data<-subset(abun, AlphaCode==speciesList[2])
speciesName<-as.character(unique(species.data$AlphaCode))

map.sesp<-ggmap(map.1, extent = "panel", maprange=TRUE,alpha=0.8)+
  geom_point(data=species.data,stat="identity",aes(x=Longitude, y=Latitude, color=NWR_code,size=Predicted+SE),alpha=0.05)+
  geom_point(data=species.data,stat="identity",aes(x=Longitude, y=Latitude, color=NWR_code,size=Predicted),alpha=0.8)+
  #scale_color_gradient(low="red",high="royalblue4",limits=c(-20,20))+
  #scale_color_manual(values=mapColors,guide=guide_legend(order=1))+
  guides(alpha=FALSE)+
  labs(x="Longitude",y="Latitude")+
  ggtitle(paste("Estimated abundance per point for ",speciesName,sep=""))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  scale_radius(guide=guide_legend(title="Abundance",order=1),range=c(0.5,7))

#NESP
species.data<-subset(abun, AlphaCode==speciesList[3])
speciesName<-as.character(unique(species.data$AlphaCode))

map.nesp<-ggmap(map.1, extent = "panel", maprange=TRUE,alpha=0.8)+
  geom_point(data=species.data,stat="identity",aes(x=Longitude, y=Latitude, color=NWR_code,size=Predicted+SE),alpha=0.05)+
  geom_point(data=species.data,stat="identity",aes(x=Longitude, y=Latitude, color=NWR_code,size=Predicted),alpha=0.8)+
  #scale_color_gradient(low="red",high="royalblue4",limits=c(-20,20))+
  #scale_color_manual(values=mapColors,guide=guide_legend(order=1))+
  guides(alpha=FALSE)+
  labs(x="Longitude",y="Latitude")+
  ggtitle(paste("Estimated abundance per point for ",speciesName,sep=""))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  scale_radius(guide=guide_legend(title="Abundance",order=1),range=c(0.5,7))

#CLRA
species.data<-subset(abun, AlphaCode==speciesList[4])
speciesName<-as.character(unique(species.data$AlphaCode))

map.clra<-ggmap(map.1, extent = "panel", maprange=TRUE,alpha=0.8)+
  geom_point(data=species.data,stat="identity",aes(x=Longitude, y=Latitude, color=NWR_code,size=Predicted+SE),alpha=0.05)+
  geom_point(data=species.data,stat="identity",aes(x=Longitude, y=Latitude, color=NWR_code,size=Predicted),alpha=0.8)+
  #scale_color_gradient(low="red",high="royalblue4",limits=c(-20,20))+
  #scale_color_manual(values=mapColors,guide=guide_legend(order=1))+
  guides(alpha=FALSE)+
  labs(x="Longitude",y="Latitude")+
  ggtitle(paste("Estimated abundance per point for ",speciesName,sep=""))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  scale_radius(guide=guide_legend(title="Abundance",order=1),range=c(0.5,7))

#WILL
species.data<-subset(abun, AlphaCode==speciesList[5])
speciesName<-as.character(unique(species.data$AlphaCode))

map.will<-ggmap(map.1, extent = "panel", maprange=TRUE,alpha=0.8)+
  geom_point(data=species.data,stat="identity",aes(x=Longitude, y=Latitude, color=NWR_code,size=Predicted+SE),alpha=0.05)+
  geom_point(data=species.data,stat="identity",aes(x=Longitude, y=Latitude, color=NWR_code,size=Predicted),alpha=0.8)+
  #scale_color_gradient(low="red",high="royalblue4",limits=c(-20,20))+
  #scale_color_manual(values=mapColors,guide=guide_legend(order=1))+
  guides(alpha=FALSE)+
  labs(x="Longitude",y="Latitude")+
  ggtitle(paste("Estimated abundance per point for ",speciesName,sep=""))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  scale_radius(guide=guide_legend(title="Abundance",order=1),range=c(0.5,7))

#TMO
species.data<-subset(abun, AlphaCode==speciesList[6])
speciesName<-as.character(unique(species.data$AlphaCode))

map.tmo<-ggmap(map.1, extent = "panel", maprange=TRUE,alpha=0.8)+
  geom_point(data=species.data,stat="identity",aes(x=Longitude, y=Latitude, color=NWR_code,size=Predicted+SE),alpha=0.05)+
  geom_point(data=species.data,stat="identity",aes(x=Longitude, y=Latitude, color=NWR_code,size=Predicted),alpha=0.8)+
  #scale_color_gradient(low="red",high="royalblue4",limits=c(-20,20))+
  #scale_color_manual(values=mapColors,guide=guide_legend(order=1))+
  guides(alpha=FALSE)+
  labs(x="Longitude",y="Latitude")+
  ggtitle(paste("Estimated abundance per point for ",speciesName,sep=""))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  scale_radius(guide=guide_legend(title="Abundance",order=1),range=c(0.5,7))


###################################################################
#make each inset fig separately

#SALS
species.data.1<-subset(species.merge.1, AlphaCode==speciesList[1])

# #reorder factor levels for NWR_code
species.data.1$NWR_code<-factor(species.data.1$NWR_code,levels= refugeList.order)

#sort species abundance by NWR then by Latitude
species.data.1<-species.data.1[order(species.data.1$NWR_code,-species.data.1$Latitude),]

#reorder factor levels for SMI_Unit
species.data.1$SMI_Unit<-factor(species.data.1$SMI_Unit, levels=rev(species.data.1$SMI_Unit))

speciesName<-as.character(unique(species.data.1$AlphaCode))

#plot abundance estimates by year and unit
fig.sals<-ggplot(data=species.data.1, aes(x=SMI_Unit, y=Predicted, ymin=Predicted, ymax=Predicted+SE))+
  coord_flip()+
  geom_errorbar(width=0, size=0.25, position=position_dodge(width=0.9),aes(color=NWR_code))+
  #geom_point(stat="identity",size=0.7,aes(color=NWR_code),alpha=1)+
  geom_bar(stat="identity",position=position_dodge(),aes(fill=NWR_code),size=0.1,alpha=0.7)+
  ggtitle(paste(speciesName," abundance by SMI unit"))+
  #theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), panel.border=element_blank())+
  theme(panel.background=element_rect(fill='white'))+
  theme(axis.text.x = element_text(size=10, color="black"),
        axis.text.y = element_text(size=6, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(angle = 90, vjust=1.2, size=15))+
  theme(panel.border=element_rect(color="black",fill=NA))+
  theme(legend.position="none")+
  lims(y=c(0,10))+
  #scale_fill_brewer(palette="Set2")+
  #scale_color_brewer(palette="Set2")+
  labs(x="SMI Unit", y="Estimated mean abundance per point")
#scale_y_continuous(expand = c(0,0), limits = c(0, max(species.abun$Predicted+species.abun$SE)+0.5))
# guides(fill = guide_legend(reverse = TRUE), color=guide_legend(reverse = TRUE))
#abun.plot.out<-plot.abun+facet_wrap(~Year,ncol=plot.years)
fig.sals

print(fig.sals)

#SESP
species.data.1<-subset(species.merge.1, AlphaCode==speciesList[2])

# #reorder factor levels for NWR_code
species.data.1$NWR_code<-factor(species.data.1$NWR_code,levels= refugeList.order)

#sort species abundance by NWR then by Latitude
species.data.1<-species.data.1[order(species.data.1$NWR_code,-species.data.1$Latitude),]

#reorder factor levels for SMI_Unit
species.data.1$SMI_Unit<-factor(species.data.1$SMI_Unit, levels=rev(species.data.1$SMI_Unit))

speciesName<-as.character(unique(species.data.1$AlphaCode))

#plot abundance estimates by year and unit
fig.sesp<-ggplot(data=species.data.1, aes(x=SMI_Unit, y=Predicted, ymin=Predicted, ymax=Predicted+SE))+
  coord_flip()+
  geom_errorbar(width=0, size=0.25, position=position_dodge(width=0.9),aes(color=NWR_code))+
  #geom_point(stat="identity",size=0.7,aes(color=NWR_code),alpha=1)+
  geom_bar(stat="identity",position=position_dodge(),aes(fill=NWR_code),size=0.1,alpha=0.7)+
  ggtitle(paste(speciesName," abundance by SMI unit"))+
  #theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), panel.border=element_blank())+
  theme(panel.background=element_rect(fill='white'))+
  theme(axis.text.x = element_text(size=10, color="black"),
        axis.text.y = element_text(size=6, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(angle = 90, vjust=1.2, size=15))+
  theme(panel.border=element_rect(color="black",fill=NA))+
  theme(legend.position="none")+
  lims(y=c(0,10))+
  #scale_fill_brewer(palette="Set2")+
  #scale_color_brewer(palette="Set2")+
  labs(x="SMI Unit", y="Estimated mean abundance per point")
#scale_y_continuous(expand = c(0,0), limits = c(0, max(species.abun$Predicted+species.abun$SE)+0.5))
# guides(fill = guide_legend(reverse = TRUE), color=guide_legend(reverse = TRUE))
#abun.plot.out<-plot.abun+facet_wrap(~Year,ncol=plot.years)
fig.sesp

print(fig.sesp)

#NESP
species.data.1<-subset(species.merge.1, AlphaCode==speciesList[3])

# #reorder factor levels for NWR_code
species.data.1$NWR_code<-factor(species.data.1$NWR_code,levels= refugeList.order)

#sort species abundance by NWR then by Latitude
species.data.1<-species.data.1[order(species.data.1$NWR_code,-species.data.1$Latitude),]

#reorder factor levels for SMI_Unit
species.data.1$SMI_Unit<-factor(species.data.1$SMI_Unit, levels=rev(species.data.1$SMI_Unit))

speciesName<-as.character(unique(species.data.1$AlphaCode))

#plot abundance estimates by year and unit
fig.nesp<-ggplot(data=species.data.1, aes(x=SMI_Unit, y=Predicted, ymin=Predicted, ymax=Predicted+SE))+
  coord_flip()+
  geom_errorbar(width=0, size=0.25, position=position_dodge(width=0.9),aes(color=NWR_code))+
  #geom_point(stat="identity",size=0.7,aes(color=NWR_code),alpha=1)+
  geom_bar(stat="identity",position=position_dodge(),aes(fill=NWR_code),size=0.1,alpha=0.7)+
  ggtitle(paste(speciesName," abundance by SMI unit"))+
  #theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), panel.border=element_blank())+
  theme(panel.background=element_rect(fill='white'))+
  theme(axis.text.x = element_text(size=10, color="black"),
        axis.text.y = element_text(size=6, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(angle = 90, vjust=1.2, size=15))+
  theme(panel.border=element_rect(color="black",fill=NA))+
  theme(legend.position="none")+
  lims(y=c(0,10))+
  #scale_fill_brewer(palette="Set2")+
  #scale_color_brewer(palette="Set2")+
  labs(x="SMI Unit", y="Estimated mean abundance per point")
#scale_y_continuous(expand = c(0,0), limits = c(0, max(species.abun$Predicted+species.abun$SE)+0.5))
# guides(fill = guide_legend(reverse = TRUE), color=guide_legend(reverse = TRUE))
#abun.plot.out<-plot.abun+facet_wrap(~Year,ncol=plot.years)
fig.sals

print(fig.nesp)

#CLRA
species.data.1<-subset(species.merge.1, AlphaCode==speciesList[4])

# #reorder factor levels for NWR_code
species.data.1$NWR_code<-factor(species.data.1$NWR_code,levels= refugeList.order)

#sort species abundance by NWR then by Latitude
species.data.1<-species.data.1[order(species.data.1$NWR_code,-species.data.1$Latitude),]

#reorder factor levels for SMI_Unit
species.data.1$SMI_Unit<-factor(species.data.1$SMI_Unit, levels=rev(species.data.1$SMI_Unit))

speciesName<-as.character(unique(species.data.1$AlphaCode))

#plot abundance estimates by year and unit
fig.clra<-ggplot(data=species.data.1, aes(x=SMI_Unit, y=Predicted, ymin=Predicted, ymax=Predicted+SE))+
  coord_flip()+
  geom_errorbar(width=0, size=0.25, position=position_dodge(width=0.9),aes(color=NWR_code))+
  #geom_point(stat="identity",size=0.7,aes(color=NWR_code),alpha=1)+
  geom_bar(stat="identity",position=position_dodge(),aes(fill=NWR_code),size=0.1,alpha=0.7)+
  ggtitle(paste(speciesName," abundance by SMI unit"))+
  #theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), panel.border=element_blank())+
  theme(panel.background=element_rect(fill='white'))+
  theme(axis.text.x = element_text(size=10, color="black"),
        axis.text.y = element_text(size=6, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(angle = 90, vjust=1.2, size=15))+
  theme(panel.border=element_rect(color="black",fill=NA))+
  theme(legend.position="none")+
  lims(y=c(0,10))+
  #scale_fill_brewer(palette="Set2")+
  #scale_color_brewer(palette="Set2")+
  labs(x="SMI Unit", y="Estimated mean abundance per point")
#scale_y_continuous(expand = c(0,0), limits = c(0, max(species.abun$Predicted+species.abun$SE)+0.5))
# guides(fill = guide_legend(reverse = TRUE), color=guide_legend(reverse = TRUE))
#abun.plot.out<-plot.abun+facet_wrap(~Year,ncol=plot.years)
fig.clra

print(fig.clra)

#WILL
species.data.1<-subset(species.merge.1, AlphaCode==speciesList[5])

# #reorder factor levels for NWR_code
species.data.1$NWR_code<-factor(species.data.1$NWR_code,levels= refugeList.order)

#sort species abundance by NWR then by Latitude
species.data.1<-species.data.1[order(species.data.1$NWR_code,-species.data.1$Latitude),]

#reorder factor levels for SMI_Unit
species.data.1$SMI_Unit<-factor(species.data.1$SMI_Unit, levels=rev(species.data.1$SMI_Unit))

speciesName<-as.character(unique(species.data.1$AlphaCode))

#plot abundance estimates by year and unit
fig.will<-ggplot(data=species.data.1, aes(x=SMI_Unit, y=Predicted, ymin=Predicted, ymax=Predicted+SE))+
  coord_flip()+
  geom_errorbar(width=0, size=0.25, position=position_dodge(width=0.9),aes(color=NWR_code))+
  #geom_point(stat="identity",size=0.7,aes(color=NWR_code),alpha=1)+
  geom_bar(stat="identity",position=position_dodge(),aes(fill=NWR_code),size=0.1,alpha=0.7)+
  ggtitle(paste(speciesName," abundance by SMI unit"))+
  #theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), panel.border=element_blank())+
  theme(panel.background=element_rect(fill='white'))+
  theme(axis.text.x = element_text(size=10, color="black"),
        axis.text.y = element_text(size=6, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(angle = 90, vjust=1.2, size=15))+
  theme(panel.border=element_rect(color="black",fill=NA))+
  theme(legend.position="none")+
  lims(y=c(0,10))+
  #scale_fill_brewer(palette="Set2")+
  #scale_color_brewer(palette="Set2")+
  labs(x="SMI Unit", y="Estimated mean abundance per point")
#scale_y_continuous(expand = c(0,0), limits = c(0, max(species.abun$Predicted+species.abun$SE)+0.5))
# guides(fill = guide_legend(reverse = TRUE), color=guide_legend(reverse = TRUE))
#abun.plot.out<-plot.abun+facet_wrap(~Year,ncol=plot.years)
fig.will

print(fig.will)

#TMO
species.data.1<-subset(species.merge.1, AlphaCode==speciesList[6])

# #reorder factor levels for NWR_code
species.data.1$NWR_code<-factor(species.data.1$NWR_code,levels= refugeList.order)

#sort species abundance by NWR then by Latitude
species.data.1<-species.data.1[order(species.data.1$NWR_code,-species.data.1$Latitude),]

#reorder factor levels for SMI_Unit
species.data.1$SMI_Unit<-factor(species.data.1$SMI_Unit, levels=rev(species.data.1$SMI_Unit))

speciesName<-as.character(unique(species.data.1$AlphaCode))

#plot abundance estimates by year and unit
fig.tmo<-ggplot(data=species.data.1, aes(x=SMI_Unit, y=Predicted, ymin=Predicted, ymax=Predicted+SE))+
  coord_flip()+
  geom_errorbar(width=0, size=0.25, position=position_dodge(width=0.9),aes(color=NWR_code))+
  #geom_point(stat="identity",size=0.7,aes(color=NWR_code),alpha=1)+
  geom_bar(stat="identity",position=position_dodge(),aes(fill=NWR_code),size=0.1,alpha=0.7)+
  ggtitle(paste(speciesName," abundance by SMI unit"))+
  #theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), panel.border=element_blank())+
  theme(panel.background=element_rect(fill='white'))+
  theme(axis.text.x = element_text(size=10, color="black"),
        axis.text.y = element_text(size=6, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(angle = 90, vjust=1.2, size=15))+
  theme(panel.border=element_rect(color="black",fill=NA))+
  theme(legend.position="none")+
  lims(y=c(0,10))+
  #scale_fill_brewer(palette="Set2")+
  #scale_color_brewer(palette="Set2")+
  labs(x="SMI Unit", y="Estimated mean abundance per point")
#scale_y_continuous(expand = c(0,0), limits = c(0, max(species.abun$Predicted+species.abun$SE)+0.5))
# guides(fill = guide_legend(reverse = TRUE), color=guide_legend(reverse = TRUE))
#abun.plot.out<-plot.abun+facet_wrap(~Year,ncol=plot.years)
fig.tmo

print(fig.tmo)


###################################################################
#Map-figure hybrids (maps with figs inset)
library(gridExtra)
library(grid)
library(png)


dir.create(paste(getwd(),"Results","Summary_results","Figures",sep="/"))
#All birds map with inset
png(paste(getwd(),"Results","Summary_results","Figures",'sals.map.inset.png',sep="/"))

grid.newpage()
vpb_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map
vpa_ <- viewport(width = 0.38, height = 0.38, x = 0.71, y = 0.285)  # the inset in lower right
print(map.all, vp = vpb_)
print(fig.sals, vp = vpa_)

dev.off()

###############################################################################

###############################################################################
#arange figs
plot.combine<-grid.arrange(fig1, fig2, fig3, fig4, fig5, figAll,  ncol=3,
                           widths=c(2.3, 2.3, 2.3), heights=c(0.7, 0.7, 0.7))


png(paste(getwd(),"Figures",'Maps_inset_combined.png',sep="/"),width=8,height=7,units="in",res=300)

plot_grid(plot.combine, legend, rel_widths = c(2, .35))

dev.off()

###############################################################################
