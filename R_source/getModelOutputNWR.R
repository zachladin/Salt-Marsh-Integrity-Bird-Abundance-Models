#save and plot abundance estimates
getModelOutputNWR<-function(dataIn, modelIn, speciesName){
  
  new.data<-dataIn
  refugeName<-unique(new.data$NWR_code)
  
  mod.1.out<-predict(modelIn,type="lambda",appendData=TRUE)
  
  #get columns to create table of Abundance estimates
  species.abun<-unique(mod.1.out[, c("Year","NWR_code","Predicted","SE","lower","upper")])
  
  #reorder table of results
  species.abun.table<-species.abun[order(species.abun$Year),]
  
  #save table as .csv file
  write.csv(species.abun.table, file=paste(refugeFilepath_full, speciesName,refugeName,paste(speciesName,refugeName,"abundance.by.year.and.unit.csv",sep="."),sep="/"),row.names=FALSE)
  
  #reorder factor levels for Year
  species.abun$Year<-factor(species.abun$Year, levels=rev(levels(species.abun$Year)))
  
  #sort species abundance by Year
  species.abun<-species.abun[order(species.abun$Year,decreasing=TRUE),]
  
  species.abun$Year = with(species.abun, factor(Year, levels = rev(levels(Year))))
  
  #get number of years
  plot.units<-round(length(unique(species.abun$Year))/2,1)
  
  #plot abundance estimates by year and unit
  plot.abun<-ggplot(data=species.abun, aes(x=Year, y=Predicted, ymin=Predicted-SE, ymax=Predicted+SE,group=Year))+
    stat_smooth(alpha=0.3,method="lm",color="darkgray")+
    geom_errorbar(width=0, size=1, aes(color=Year))+
    geom_line(color="black")+
    geom_point(stat="identity",size=3,aes(color=Year))+
    ggtitle(paste(speciesName," abundance in ", refugeName, " by SMI Unit and Year"))+
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
  abun.plot.out<-plot.abun+facet_wrap(~Year,ncol=plot.units)
  abun.plot.out
  print(abun.plot.out)
  
  #save plot of abundance estimates
  myFilepath<-paste(refugeFilepath_full,speciesName,refugeName,sep="/")
  ggsave(abun.plot.out, filename=paste(speciesName,refugeName,"summary.abun.fig.pdf",sep="."),path=myFilepath, width=11,height=8.5, limitsize=FALSE)
  
}

#End
