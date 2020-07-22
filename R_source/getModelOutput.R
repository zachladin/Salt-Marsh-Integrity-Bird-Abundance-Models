#save and plot abundance estimates
getModelOutput<-function(dataIn, modelIn, speciesName){

  new.data<-dataIn
  refugeName<-unique(as.character(new.data$NWR_code))

  mod.1.out<-predict(modelIn,type="lambda",appendData=TRUE)

  #get columns to create table of Abundance estimates
  species.abun<-unique(mod.1.out[, c("Year","SMI_Unit","Predicted","SE","lower","upper")])

  #add column with model formula
  model.formula<-paste(deparse(modelIn@call)[1], deparse(modelIn@call)[2],sep="")
  species.abun$model.formula<-model.formula
  
  dir.create(paste(getwd(), "Results",sep="/"))
  dir.create(paste(getwd(), "Results",speciesName,sep="/"))
  dir.create(paste(getwd(), "Results",speciesName,refugeName,sep="/"))
  
  #reorder table of results
  species.abun.table<-species.abun[order(species.abun$SMI_Unit),]

  #save table as .csv file
  write.csv(species.abun.table, file=paste(getwd(), "Results", speciesName,refugeName,paste(speciesName,refugeName,"abundance.by.year.and.unit.csv",sep="."),sep="/"),row.names=FALSE)

  #save detection probs
  det.probs<-unique(getP(modelIn))
  write.csv(det.probs,file=paste(getwd(), "Results", speciesName,refugeName,paste(speciesName,refugeName,"detection.by.visit.csv",sep="."),sep="/"),row.names=FALSE)

  #save model information and coefficients
  mod.coefs<-coef(modelIn)
  dput( list(coefs=mod.coefs),
        file=paste(getwd(), "Results",speciesName,refugeName, paste(speciesName,"_",refugeName,"_","model_coefficients",".R", sep=''),sep="/")   )

  mod.coefs.df<-as.data.frame(mod.coefs)
  mod.coefs.df.2<-data.frame(NWR_code=refugeName,Species=speciesName,Coefficient=row.names(mod.coefs.df), Value=mod.coefs.df$mod.coefs,Model=model.formula)

  #now save for later
  write.csv(mod.coefs.df.2, file=paste(getwd(), "Results", speciesName,refugeName,paste(speciesName,refugeName,"coefficients.by.unit.csv",sep="."),sep="/"),row.names=FALSE)


  #NOW PLOT
  #reorder factor levels for SMI_Unit
  species.abun$SMI_Unit<-factor(species.abun$SMI_Unit, levels=rev(levels(species.abun$SMI_Unit)))

  #sort species abundance by SMI_Unit
  species.abun<-species.abun[order(species.abun$SMI_Unit,decreasing=TRUE),]

  species.abun$SMI_Unit = with(species.abun, factor(SMI_Unit, levels = rev(levels(SMI_Unit))))

  #get number of years
  plot.units<-if((length(unique(species.abun$SMI_Unit)) < 7)==TRUE){
      round(length(unique(species.abun$SMI_Unit))/2,1)
  }else{
    round(length(unique(species.abun$SMI_Unit))/4,1)
  }

  #plot abundance estimates by year and unit
  plot.abun<-ggplot(data=species.abun, aes(x=Year, y=Predicted, ymin=Predicted-SE, ymax=Predicted+SE,group=SMI_Unit))+
    stat_smooth(alpha=0.7,method="lm",color="white")+
    geom_errorbar(width=0, size=0.75, aes(color=SMI_Unit))+
    geom_line(color=I("grey20"))+
    geom_point(stat="identity",size=3,aes(color=SMI_Unit))+
    ggtitle(paste(speciesName," abundance in ", refugeName, " by SMI Unit and Year"))+
    #theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), panel.border=element_blank())+
    #theme(panel.background=element_rect(fill='white'))+
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5,vjust=0.5, size=10, color="black"),
          axis.text.y = element_text(size=10, color="black"),
          axis.title.x = element_text(size=15, hjust=0.5, vjust=0.2),
          axis.title.y = element_text(angle = 90, vjust=1.2, size=15))+
    theme(panel.border=element_rect(color="black",fill=NA))+
    theme(legend.position="none")+
    #theme(axis.ticks.x=element_blank(), axis.text.x = element_blank())+
#     scale_fill_brewer(palette="Dark2")+
#     scale_color_brewer(palette="Dark2")+
    labs(x="Year", y="Estimated abundance")
  abun.plot.out<-plot.abun+facet_wrap(~SMI_Unit,ncol=plot.units)
  abun.plot.out
  print(abun.plot.out)

  #save plot of abundance estimates
  myFilepath<-paste(getwd(), "Results",speciesName,refugeName,sep="/")
  ggsave(abun.plot.out, filename=paste(speciesName,refugeName,"summary.abun.fig.pdf",sep="."),path=myFilepath, width=7.5,height=5, limitsize=FALSE)
  ggsave(abun.plot.out, filename=paste(speciesName,refugeName,"summary.abun.fig.png",sep="."),path=myFilepath, width=7.5,height=5, limitsize=FALSE)

}

#End
