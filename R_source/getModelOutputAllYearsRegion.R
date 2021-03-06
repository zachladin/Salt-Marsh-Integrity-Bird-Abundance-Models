#plot abundance estiates
getModelOutputAllYearsRegion<-function(dataIn, modIn, speciesName){

  new.data<-dataIn
  regionName<-unique(new.data$RegionName)

  mod<-modIn

  mod.1.out<-predict(mod,type="lambda",appendData=TRUE)

  #get columns to create table of Abundance estimates
  species.abun<-unique(mod.1.out[, c("NWR_code","Predicted","SE","lower","upper")])

  #add column with model formula
  model.formula<-paste(deparse(mod@call)[1], deparse(mod@call)[2],sep="")
  species.abun$model.formula<-model.formula

  #reorder table of results
  species.abun.table<-species.abun[order(species.abun$NWR_code),]

  #save table as .csv file
  write.csv(species.abun.table, file=paste(regionFilepath_full, speciesName,regionName,paste(speciesName,"abundance.by.NWR.csv",sep="."),sep="/"),row.names=FALSE)

  #reorder factor levels for NWR_code
  species.abun$NWR_code<-factor(species.abun$NWR_code, levels=rev(levels(species.abun$NWR_code)))

  #sort species abundance by NWR_code
  species.abun<-species.abun[order(species.abun$NWR_code,decreasing=TRUE),]

  species.abun$NWR_code = with(species.abun, factor(NWR_code, levels = rev(levels(NWR_code))))

  myColors<-c("steelblue4","darkgoldenrod2","darkred","darkcyan")

  #plot abundance estimates by year and unit
  plot.abun<-ggplot(data=species.abun, aes(x=NWR_code, y=Predicted, ymin=Predicted-SE, ymax=Predicted+SE))+
    geom_errorbar(width=0, size=1, aes(color=NWR_code))+
    geom_point(stat="identity",size=4,aes(color=NWR_code),alpha=1)+
    ggtitle(paste(speciesName," in Region ", regionName,sep=""))+
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), panel.border=element_blank())+
    theme(panel.background=element_rect(fill='white'))+
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5, size=14, color="black"),
          axis.text.y = element_text(size=14, color="black"),
          axis.title.x = element_text(size=17, hjust=0.5, vjust=0.2),
          axis.title.y = element_text(angle = 90, vjust=1.2, size=17))+
    theme(panel.border=element_rect(color="black",fill=NA))+
    #scale_fill_brewer(palette="Set2")+
    scale_color_brewer(palette="Dark2")+
    labs(x="USFWS National Wildlife Refuge", y="Estimated abundance")
    #scale_y_continuous(expand = c(0,0), limits = c(0, max(species.abun$Predicted+species.abun$SE)+0.5))
  # guides(fill = guide_legend(reverse = TRUE), color=guide_legend(reverse = TRUE))
  #abun.plot.out<-plot.abun+facet_wrap(~Year,ncol=plot.years)
  plot.abun
  print(plot.abun)


  #save plot of abundance estimates
  myFilepath<-paste(regionFilepath_full,speciesName,regionName,sep="/")
  ggsave(plot.abun, filename=paste(speciesName,regionName,"nwr.summary.abun.fig.pdf",sep="."),path=myFilepath, width=7,height=5, limitsize=FALSE)

}

#End