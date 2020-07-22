#save and plot abundance estimates
getModelOutputRegionHexagon<-function(dataIn, modelIn, speciesName){

  new.data<-dataIn
  regionName<-unique(new.data$RegionName)

  mod.1.out<-predict(modelIn,type="lambda",appendData=TRUE)

  #get columns to create table of Abundance estimates
  species.abun<-unique(mod.1.out[, c("Year","Hexagon","Predicted","SE","lower","upper")])

  #reorder table of results
  species.abun.table<-species.abun[order(species.abun$Hexagon),]

  #save table as .csv file
  write.csv(species.abun.table, file=paste(regionFilepath_full, speciesName,regionName,paste(speciesName,"abundance.by.year.and.NWR.csv",sep="."),sep="/"),row.names=FALSE)

  #reorder factor levels for Hexagon
  species.abun$Hexagon<-factor(species.abun$Hexagon, levels=rev(levels(species.abun$Hexagon)))

  #sort species abundance by Hexagon
  species.abun<-species.abun[order(species.abun$Hexagon,decreasing=TRUE),]

  species.abun$Hexagon = with(species.abun, factor(Hexagon, levels = rev(levels(Hexagon))))

  #get number of years
  plot.units<-round(length(unique(species.abun$Hexagon))/2,1)

  #plot abundance estimates by year and unit
  plot.abun<-ggplot(data=species.abun, aes(x=Year, y=Predicted, ymin=Predicted-SE, ymax=Predicted+SE,group=Hexagon))+
    stat_smooth(alpha=0.3,method="lm",color="darkgray")+
    geom_errorbar(width=0, size=1, aes(color=Hexagon))+
    geom_line(color="black")+
    geom_point(stat="identity",size=3,aes(color=Hexagon))+
    ggtitle(paste(speciesName," abundance in Region ", regionName, " by NWR and Year"))+
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), panel.border=element_blank())+
    theme(panel.background=element_rect(fill='white'))+
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5, size=14, color="black"),
          axis.text.y = element_text(size=10, color="black"),
          axis.title.x = element_text(size=17, hjust=0.5, vjust=0.2),
          axis.title.y = element_text(angle = 90, vjust=1.2, size=17))+
    theme(panel.border=element_rect(color="black",fill=NA))+
    #theme(axis.ticks.x=element_blank(), axis.text.x = element_blank())+
#     scale_fill_brewer(palette="Dark2")+
#     scale_color_brewer(palette="Dark2")+
    labs(x="Year", y="Estimated abundance")
  abun.plot.out<-plot.abun+facet_wrap(~Hexagon,ncol=plot.units)
  abun.plot.out
  print(abun.plot.out)

  #save plot of abundance estimates
  myFilepath<-paste(regionFilepath_full,speciesName,regionName,sep="/")
  ggsave(abun.plot.out, filename=paste(speciesName,regionName,"summary.abun.fig.pdf",sep="."),path=myFilepath, width=11,height=8.5, limitsize=FALSE)

}

#End
