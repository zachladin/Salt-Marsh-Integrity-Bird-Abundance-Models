#plot abundance estiates
getModelOutputAllYears<-function(dataIn, modelIn, speciesName){

  new.data<-dataIn
  refugeName<-as.character(unique(new.data$NWR_code))

  mod.1.out<-predict(modelIn,type="lambda",appendData=TRUE)

  #get columns to create table of Abundance estimates
  species.abun<-unique(mod.1.out[, c("SMI_Unit","Predicted","SE","lower","upper","Longitude","Latitude")])

  #reorder table of results
  species.abun.table<-species.abun[order(species.abun$Latitude,decreasing=TRUE),]

  #add column with model formula
  model.formula<-paste(deparse(modelIn@call)[1], deparse(modelIn@call)[2],sep="")
  species.abun.table$model.formula<-model.formula

  dir.create(paste(getwd(), "Results",sep="/"))
  dir.create(paste(getwd(), "Results",speciesName,sep="/"))
  dir.create(paste(getwd(), "Results",speciesName,refugeName,sep="/"))

  #save table as .csv file
  write.csv(species.abun.table, file=paste(getwd(), "Results", speciesName,refugeName,paste(speciesName,refugeName,"abundance.by.unit.csv",sep="."),sep="/"),row.names=FALSE)

  #save detection probs
  det.probs<-unique(getP(modelIn))
  write.csv(det.probs,file=paste(getwd(), "Results", speciesName,refugeName,paste(speciesName,refugeName,"detection.by.visit.csv",sep="."),sep="/"),row.names=FALSE)

  #save model information and coefficients
  mod.coefs<-coef(modelIn)
  dput( list(coefs=mod.coefs),
        file=paste(getwd(), "Results",speciesName,refugeName, paste(speciesName,"_",refugeName,"_","model_coefficients_allYears",".R", sep=''),sep="/")   )

  mod.coefs.df<-as.data.frame(mod.coefs)
  mod.coefs.df.2<-data.frame(NWR_code=refugeName,Species=speciesName,Coefficient=row.names(mod.coefs.df), Value=mod.coefs.df$mod.coefs,Model=model.formula)

  #now save for later
  write.csv(mod.coefs.df.2, file=paste(getwd(), "Results", speciesName,refugeName,paste(speciesName,refugeName,"coefficients.allYears.by.unit.csv",sep="."),sep="/"),row.names=FALSE)

  #sort species abundance by SMI_Unit
  species.abun<-species.abun[order(species.abun$Latitude,decreasing=TRUE),]

  #reorder factor levels for SMI_Unit
  species.abun$SMI_Unit<-factor(species.abun$SMI_Unit, levels=rev(species.abun$SMI_Unit))


  #plot abundance estimates by year and unit
  plot.abun<-ggplot(data=species.abun, aes(x=SMI_Unit, y=Predicted, ymin=Predicted-SE, ymax=Predicted+SE))+
    coord_flip()+
    geom_errorbar(width=0, size=1, aes(color=SMI_Unit))+
    geom_point(stat="identity",size=4,aes(color=SMI_Unit),alpha=1)+
    ggtitle(paste(speciesName," abundance in ", refugeName, " by SMI Unit"))+
    #theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), panel.border=element_blank())+
    #theme(panel.background=element_rect(fill='white'))+
    theme(axis.text.x = element_text(size=10, color="black"),
          axis.text.y = element_text(size=10, color="black"),
          axis.title.x = element_text(size=15, hjust=0.5, vjust=0.2),
          axis.title.y = element_text(angle = 90, vjust=1.2, size=15))+
    theme(panel.border=element_rect(color="black",fill=NA))+
    theme(legend.position="none")+
    #scale_fill_brewer(palette="Set2")+
    #scale_color_brewer(palette="Set2")+
    labs(x="SMI Unit", y="Estimated mean abundance per point")
    #scale_y_continuous(expand = c(0,0), limits = c(0, max(species.abun$Predicted+species.abun$SE)+0.5))
  # guides(fill = guide_legend(reverse = TRUE), color=guide_legend(reverse = TRUE))
  #abun.plot.out<-plot.abun+facet_wrap(~Year,ncol=plot.years)
  plot.abun
  print(plot.abun)


  #save plot of abundance estimates
  myFilepath<-paste(getwd(), "Results",speciesName,refugeName,sep="/")
  ggsave(plot.abun, filename=paste(speciesName,refugeName,"unit.summary.abun.fig.pdf",sep="."),path=myFilepath, width=6,height=7, limitsize=FALSE)
  ggsave(plot.abun, filename=paste(speciesName,refugeName,"unit.summary.abun.fig.png",sep="."),path=myFilepath, width=6,height=7, limitsize=FALSE)

  return(species.abun.table)
}

#End