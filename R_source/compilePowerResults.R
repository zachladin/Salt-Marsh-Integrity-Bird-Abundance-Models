#compilePowerResults function
compilePowerResults<-function(dataIn, speciesName){

  data=dataIn

  #set working directory to get data
  orig.dir<-paste("/Users/Zach/Dropbox (ZachTeam)",paste("SMI_Power_",speciesName,sep=""),sep="/")
  setwd(orig.dir)

  #create dir to save results
  dir.create(path=paste(getwd(), "Results","Summary_Power_Results",sep="/"))

  #Summary_Bayes_Power
  dir.create(path=paste(paste(getwd(), "Results","Summary_Power_Results",sep="/"),"Summary_Bayes_Power",sep="/"))


  ###############################################################################################################
  #get refugeList
  refugeList<-sort(unique(as.character(data$NWR_code)))
  refugeList

  #########################################################################################
  #get scenarioList
  scenarioList<-c("Full_annual","Full_biennial","Full_triennial","Half_A_annual","Half_A_biennial","Half_A_triennial",
                  "Half_B_annual","Half_B_biennial","Half_B_triennial", "Third_A_annual","Third_A_biennial","Third_A_triennial",
                  "Third_B_annual","Third_B_biennial","Third_B_triennial", "Third_C_annual","Third_C_biennial","Third_C_triennial")
  #########################################################################################
  #get scenarioGroup
  scenarioGroupList<-c("SMI_full","SMI_half_A","SMI_half_B","SMI_third_A","SMI_third_B","SMI_third_C")

  #subGroupList
  subGroupList<-c("Full","Half","Thrid")
  #########################################################################################
  #get scenarioGroup
  samplingTypeList<-c("_annual","_biennial","_triennial")

  #########################################################################################
  #get lists for looping
  refugeList<-sort(unique(as.character(data$NWR_code)))
  visitList<-c(1, 2, 3)
  yearList<-c(5,10)
  declineList<-c("-0.01","-0.05","-0.10","-0.30","-0.50")

  #speciesList<-c("SALS","NESP","SESP","CLRA","WILL","TMO")
  speciesList<-speciesName
  ###############################################################################################################
  #summarize Bayes Power
  for(q in 1:length(speciesList)){
    message("Compiling power results")

    pwr.results.out.all<-list()
    for(i in 1:length(refugeList)){
      #foreach(i=length(refugeList),.combine='rbind') %dopar%{
      print(paste(refugeList[i]))

      pwr.results.out.scenarioGroup<-list()
      for(k in 1:length(scenarioGroupList)){
        print(paste(refugeList[i],scenarioGroupList[k],sep="_"))

        pwr.results.out.samplingType<-list()
        for(h in 1:length(samplingTypeList)){
          print(paste(refugeList[i],scenarioGroupList[k],samplingTypeList[h],sep="_"))

          pwr.results.out.scenario<-list()
          for(j in 1:length(scenarioList)){
            print(paste(refugeList[i],scenarioGroupList[k],samplingTypeList[h],scenarioList[j],sep="_"))

            pwr.results.out.decline<-list()
            for(p in 1:length(declineList)){
              print(paste(refugeList[i],scenarioGroupList[k],samplingTypeList[h],scenarioList[j],declineList[p],sep="_"))

              pwr.results.out.year<-list()
              for(y in 1:length(yearList)){
                print(paste(refugeList[i],scenarioGroupList[k],samplingTypeList[h],scenarioList[j],declineList[p],yearList[y],sep="_"))

                pwr.results.out.visit<-list()
                for(z in 1:length(visitList)){
                  print(paste(refugeList[i],scenarioGroupList[k],samplingTypeList[h],scenarioList[j],declineList[p],yearList[y],visitList[z],sep="_"))


                  new.data<-tryCatch(
                    {
                      #set working directory to harvest power results
                      setwd(paste(orig.dir,"Results", scenarioGroupList[k], paste("SMI_",scenarioList[j],sep=""),"Power_Bayes",speciesList[q],refugeList[i],
                                  paste(scenarioList[j],"Power_", speciesList[q], "_", refugeList[i],"_", declineList[p], "decline","_",
                                        yearList[y],"years","_",visitList[z],"visits",sep=""),sep="/"))

                      #read in power table results
                      pwr.table<-read.csv("power.table.out.csv")
                      pwr.table$Model<-"Bayesian_power"
                      pwr.table$Species<-speciesList[q]
                      pwr.table$Refuge<-refugeList[i]
                      pwr.table$Decline<-declineList[p]
                      pwr.table$Years<-yearList[y]
                      pwr.table$Scenario<-scenarioList[j]


                      pwr.table.out<-pwr.table[,c("Model","model_Type","Species", "Refuge","SMI_Unit","unitNum","NumPoints","Decline","nVisits","Years","Scenario",
                                                  "Predicted.abun","abun_lower2.5." ,"abun_upper97.5.","Mean_gamma","gamma_lower2.5.","gamma_upper97.5.","rmse","BiasMean",
                                                  "CI","significance","pwr")]
                      cond1<-data.frame(pwr.table.out)
                      cond1
                    },error= function(cond2){
                      cond2<-data.frame(Model="Bayesian_power", model_Type=paste("Bayes_power_", speciesList[q], "_", refugeList[i],"_", declineList[j],"decline_", yearList[y],"years_",visitList[z],"visits",sep=""),
                                        Species=speciesList[q], Refuge=refugeList[i], SMI_Unit=NA, unitNum=NA, NumPoints=NA, Decline=declineList[j], nVisits=visitList[z], Years=yearList[y], Scenario=scenarioList[j],
                                        Predicted.abun=NA, abun_lower2.5.=NA, abun_upper97.5.= NA, Mean_gamma= NA, gamma_lower2.5.= NA, gamma_upper97.5.= NA,
                                        rmse=NA, BiasMean=NA, CI=NA,significance=NA, pwr=NA)
                      #cond2<-NULL
                      cond2
                    }
                  )

                  pwr.results.out.visit<-rbind(pwr.results.out.visit,new.data)
                }
                pwr.results.out.year<-rbind(pwr.results.out.year, pwr.results.out.visit)
              }
              pwr.results.out.decline<-rbind(pwr.results.out.decline, pwr.results.out.year)
            }
            pwr.results.out.scenario<-rbind(pwr.results.out.decline, pwr.results.out.scenario)
          }
          pwr.results.out.samplingType<-rbind(pwr.results.out.samplingType, pwr.results.out.scenario)
        }
        pwr.results.out.scenarioGroup<-rbind(pwr.results.out.scenarioGroup, pwr.results.out.samplingType)
      }
      pwr.results.out.all<-rbind(pwr.results.out.all, pwr.results.out.scenarioGroup)
    }

    setwd(paste("/Users/Zach/Dropbox (ZachTeam)",paste("SMI_Power_",speciesList[q],sep=""),"Results",sep="/"))
    message("Saving power results.")
    write.csv(pwr.results.out.all, paste(getwd(), "Summary_Power_Results", paste(speciesList[q],"Bayes_power_AllRefuges_summary.csv",sep="_"),sep="/"),row.names=FALSE)
  }
}

