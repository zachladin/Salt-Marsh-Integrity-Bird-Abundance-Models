#load packages and source files
#function to load required R packages and source files

loadPackages<-function(){
  message("Installing R packages.")


  message("Loading R packages.")

  require(reshape)
  require(plyr)
  require(unmarked)
  require(rjags)
  require(arm)
  require(ggplot2)
  require(RColorBrewer)
  #require(pwr)
  #require(maptools)
  #require(spdep)
  require(doParallel)
  require(foreach)
  require(dclone)
  require(data.table)


  message("Loading R source files containing functions.")

  #load source files with all fuctions
  orig.dir<-getwd()
  setwd(paste(getwd(),"R_source",sep="/"))

  source("summaryFunction.R")
  source("compilePowerResults.R")
  source("formatRawData.R")
  source("formatRawDataSharp.R")
  source("makeGMMumf.R")
  source("makeMultPoisumf.R")
  source("makeGMMumfRegion.R")
  source("getStateData.R")
  source("getRefugeData.R")
  source("getHexagonData.R")
  source("getRegionData.R")
  source("runUnmarked.R")
  source("runUnmarkedNWR.R")
  source("runUnmarkedAllYears.R")
  source("runUnmarkedAllYears_multiPois.R")
  source("runUnmarkedHexagon.R")
  source("runUnmarkedAllYearsHexagon.R")
  source("runUnmarkedRegion.R")
  source("runUnmarkedAllYearsRegion.R")
  source("runUnmarkedRegionHexagon.R")
  source("runUnmarkedAllYearsRegionHexagon.R")
  source("runUnmarkedStateHexagon.R")
  source("runUnmarkedAllYearsStateHexagon.R")
  source("runUnmarkedAllYearsNWR.R")
  source("getModelOutputNWR.R")
  source("getModelOutputAllYearsNWR.R")
  source("getModelOutputHexagon.R")
  source("getModelOutputAllYearsHexagon.R")
  source("getModelOutputRegion.R")
  source("getModelOutputRegionHexagon.R")
  source("runUnmarkedRegionAllSHARP.R")
  source("getModelOutput.R")
  source("getModelOutputTableAllYearsRegionSHARP.R")
  source("getModelOutputTableRegionSHARP.R")
  source("runUnmarkedAllYearsRegionAllSHARP.R")
  source("getModelOutputAllYearsRegionSHARP.R")
  source("runUnmarkedAllYearsRegionSHARP.R")
  source("runUnmarkedAllYearsRegionHexagonSHARP.R")
  source("getAbundanceRegionHexagonSHARP.R")
  source("getModelOutputAllYears.R")
  source("getModelOutputAllYears_multiPois.R")
  source("getModelOutputAllYearsRegion.R")
  source("getModelOutputAllYearsRegionHexagon.R")
  source("getAbundance.R")
  source("getAbundanceSMI.R")
  source("getAbundanceSMI_multiPois.R")
  source("getAbundanceHexagon.R")
  source("getAbundanceRegion.R")
  source("getAbundanceNWR.R")
  source("getAbundanceRegionHexagon.R")
  source("dataSimFullPanel.R")
  source("dataSimRegionFull.R")
  source("dataSimUnitsFull.R")
  source("dataSimUnits_annual.R")
  source("dataSimUnits_biennial.R")
  source("dataSimUnits_triennial.R")
  source("dataSimUnitsFull_allYears.R")
  source("runJAGSmodelUnits.R")
  source("runJAGSmodelUnits_Visit.R")
  source("runJAGSmodelUnits_Visit_allYears.R")
  source("runJAGSmodelUnits_Visit_Observer.R")
  source("runJAGSmodelNWR.R")
  source("divideUnitsHalf.R")
  source("divideUnitsThirds.R")
  source("divideUnitsThirdsCore.R")
  source("dividePointsHalf.R")
  source("dividePointsThirds.R")
  source("getCoreUnits.R")
  source("createFolders.R")

  setwd(orig.dir)

  message("Creating filepaths.")
  #define some filepaths
####################################################################################################
  #Data
  dataFilepath<<-paste(orig.dir,"Data",sep="/")
  refugeDataFilepath_full<<-paste(dataFilepath,"Data_refuge_full",sep="/")
  refugeDataFilepath_half_A<<-paste(dataFilepath,"Data_refuge_half_A",sep="/")
  refugeDataFilepath_half_B<<-paste(dataFilepath,"Data_refuge_half_B",sep="/")
  refugeDataFilepath_third_A<<-paste(dataFilepath,"Data_refuge_third_A",sep="/")
  refugeDataFilepath_third_B<<-paste(dataFilepath,"Data_refuge_third_B",sep="/")
  refugeDataFilepath_third_C<<-paste(dataFilepath,"Data_refuge_third_C",sep="/")

  speciesUMFdataFilepath<<-paste(getwd(),"Data","Data_spp_umf",sep="/")

  speciesUMFdataFilepath_full<<-paste(getwd(),"Data","Data_spp_umf_full",sep="/")
  speciesUMFdataFilepath_half_A<<-paste(getwd(),"Data","Data_spp_umf_half_A",sep="/")
  speciesUMFdataFilepath_half_B<<-paste(getwd(),"Data","Data_spp_umf_half_B",sep="/")
  speciesUMFdataFilepath_third_A<<-paste(getwd(),"Data","Data_spp_umf_third_A",sep="/")
  speciesUMFdataFilepath_third_B<<-paste(getwd(),"Data","Data_spp_umf_third_B",sep="/")
  speciesUMFdataFilepath_third_C<<-paste(getwd(),"Data","Data_spp_umf_third_C",sep="/")

####################################################################################################
  #Results filepath
  resultsFilepath<<-paste(orig.dir,"Results",sep="/")
#################################################################################################
  #Hexagon filepath
  
  #Full - filepaths
  hexagonFilepath_full<<-paste(orig.dir, "Results","Hexagon",sep="/")
  
  #create fild
  dir.create(hexagonFilepath_full)
  ####################################################################################################
  
  #Full - filepaths
  #annual
  SMIfullFilepath<<-paste(orig.dir, "Results","SMI_full",sep="/")
    SMIfullFilepath_annual<<-paste(SMIfullFilepath, "SMI_full_annual",sep="/")
      refugeFilepath_full_annual<<-paste(SMIfullFilepath_annual,"Abundance_refuge",sep="/")
      bayesPwrFilepath_full_annual<<-paste(SMIfullFilepath_annual,"Power_Bayes",sep="/")
  #biennial
    SMIfullFilepath_biennial<<-paste(SMIfullFilepath, "SMI_full_biennial",sep="/")
      refugeFilepath_full_biennial<<-paste(SMIfullFilepath_biennial,"Abundance_refuge",sep="/")
      bayesPwrFilepath_full_biennial<<-paste(SMIfullFilepath_biennial,"Power_Bayes",sep="/")
  #triennial
    SMIfullFilepath_triennial<<-paste(SMIfullFilepath, "SMI_full_triennial",sep="/")
      refugeFilepath_full_triennial<<-paste(SMIfullFilepath_triennial,"Abundance_refuge",sep="/")
      bayesPwrFilepath_full_triennial<<-paste(SMIfullFilepath_triennial,"Power_Bayes",sep="/")

####################################################################################################

#Half_A - filepaths
  #annual
  SMIhalf_AFilepath<<-paste(orig.dir, "Results","SMI_half_A",sep="/")
    SMIhalf_AFilepath_annual<<-paste(SMIhalf_AFilepath, "SMI_half_A_annual",sep="/")
      refugeFilepath_half_A_annual<<-paste(SMIhalf_AFilepath_annual,"Abundance_refuge",sep="/")
      bayesPwrFilepath_half_A_annual<<-paste(SMIhalf_AFilepath_annual,"Power_Bayes",sep="/")
  #biennial
  SMIhalf_AFilepath_biennial<-paste(SMIhalf_AFilepath, "SMI_half_A_biennial",sep="/")
      refugeFilepath_half_A_biennial<<-paste(SMIhalf_AFilepath_biennial,"Abundance_refuge",sep="/")
      bayesPwrFilepath_half_A_biennial<<-paste(SMIhalf_AFilepath_biennial,"Power_Bayes",sep="/")
  #triennial
  SMIhalf_AFilepath_triennial<<-paste(SMIhalf_AFilepath, "SMI_half_A_triennial",sep="/")
      refugeFilepath_half_A_triennial<<-paste(SMIhalf_AFilepath_triennial,"Abundance_refuge",sep="/")
      bayesPwrFilepath_half_A_triennial<<-paste(SMIhalf_AFilepath_triennial,"Power_Bayes",sep="/")

#Half_B - filepaths
  #annual
  SMIhalf_BFilepath<<-paste(orig.dir, "Results","SMI_half_B",sep="/")
      SMIhalf_BFilepath_annual<<-paste(SMIhalf_BFilepath, "SMI_half_B_annual",sep="/")
      refugeFilepath_half_B_annual<<-paste(SMIhalf_BFilepath_annual,"Abundance_refuge",sep="/")
      bayesPwrFilepath_half_B_annual<<-paste(SMIhalf_BFilepath_annual,"Power_Bayes",sep="/")
  #biennial
  SMIhalf_BFilepath_biennial<<-paste(SMIhalf_BFilepath, "SMI_half_B_biennial",sep="/")
      refugeFilepath_half_B_biennial<<-paste(SMIhalf_BFilepath_biennial,"Abundance_refuge",sep="/")
      bayesPwrFilepath_half_B_biennial<<-paste(SMIhalf_BFilepath_biennial,"Power_Bayes",sep="/")
  #triennial
  SMIhalf_BFilepath_triennial<<-paste(SMIhalf_BFilepath, "SMI_half_B_triennial",sep="/")
      refugeFilepath_half_B_triennial<<-paste(SMIhalf_BFilepath_triennial,"Abundance_refuge",sep="/")
      bayesPwrFilepath_half_B_triennial<<-paste(SMIhalf_BFilepath_triennial,"Power_Bayes",sep="/")

####################################################################################################
#Third_A - filepaths
  #annual
  SMIthird_AFilepath<<-paste(orig.dir, "Results","SMI_third_A",sep="/")
      SMIthird_AFilepath_annual<<-paste(SMIthird_AFilepath, "SMI_third_A_annual",sep="/")
      refugeFilepath_third_A_annual<<-paste(SMIthird_AFilepath_annual,"Abundance_refuge",sep="/")
      bayesPwrFilepath_third_A_annual<<-paste(SMIthird_AFilepath_annual,"Power_Bayes",sep="/")
  #biennial
  SMIthird_AFilepath_biennial<<-paste(SMIthird_AFilepath, "SMI_third_A_biennial",sep="/")
      refugeFilepath_third_A_biennial<<-paste(SMIthird_AFilepath_biennial,"Abundance_refuge",sep="/")
      bayesPwrFilepath_third_A_biennial<<-paste(SMIthird_AFilepath_biennial,"Power_Bayes",sep="/")
  #triennial
  SMIthird_AFilepath_triennial<<-paste(SMIthird_AFilepath, "SMI_third_A_triennial",sep="/")
      refugeFilepath_third_A_triennial<<-paste(SMIthird_AFilepath_triennial,"Abundance_refuge",sep="/")
      bayesPwrFilepath_third_A_triennial<<-paste(SMIthird_AFilepath_triennial,"Power_Bayes",sep="/")

#Third_B - filepaths
  #annual
  SMIthird_BFilepath<<-paste(orig.dir, "Results","SMI_third_B",sep="/")
      SMIthird_BFilepath_annual<<-paste(SMIthird_BFilepath, "SMI_third_B_annual",sep="/")
      refugeFilepath_third_B_annual<<-paste(SMIthird_BFilepath_annual,"Abundance_refuge",sep="/")
      bayesPwrFilepath_third_B_annual<<-paste(SMIthird_BFilepath_annual,"Power_Bayes",sep="/")
  #biennial
  SMIthird_BFilepath_biennial<<-paste(SMIthird_BFilepath, "SMI_third_B_biennial",sep="/")
      refugeFilepath_third_B_biennial<<-paste(SMIthird_BFilepath_biennial,"Abundance_refuge",sep="/")
      bayesPwrFilepath_third_B_biennial<<-paste(SMIthird_BFilepath_biennial,"Power_Bayes",sep="/")
  #triennial
  SMIthird_BFilepath_triennial<<-paste(SMIthird_BFilepath, "SMI_third_B_triennial",sep="/")
      refugeFilepath_third_B_triennial<<-paste(SMIthird_BFilepath_triennial,"Abundance_refuge",sep="/")
      bayesPwrFilepath_third_B_triennial<<-paste(SMIthird_BFilepath_triennial,"Power_Bayes",sep="/")

#Third_C - filepaths
  #annual
  SMIthird_CFilepath<<-paste(orig.dir, "Results","SMI_third_C",sep="/")
      SMIthird_CFilepath_annual<<-paste(SMIthird_CFilepath, "SMI_third_C_annual",sep="/")
      refugeFilepath_third_C_annual<<-paste(SMIthird_CFilepath_annual,"Abundance_refuge",sep="/")
      bayesPwrFilepath_third_C_annual<<-paste(SMIthird_CFilepath_annual,"Power_Bayes",sep="/")
  #biennial
  SMIthird_CFilepath_biennial<<-paste(SMIthird_CFilepath, "SMI_third_C_biennial",sep="/")
      refugeFilepath_third_C_biennial<<-paste(SMIthird_CFilepath_biennial,"Abundance_refuge",sep="/")
      bayesPwrFilepath_third_C_biennial<<-paste(SMIthird_CFilepath_biennial,"Power_Bayes",sep="/")
  #triennial
  SMIthird_CFilepath_triennial<<-paste(SMIthird_CFilepath, "SMI_third_C_triennial",sep="/")
      refugeFilepath_third_C_triennial<<-paste(SMIthird_CFilepath_triennial,"Abundance_refuge",sep="/")
      bayesPwrFilepath_third_C_triennial<<-paste(SMIthird_CFilepath_triennial,"Power_Bayes",sep="/")

####################################################################################################
#Write JAGS model NO spatial autocorrelation term (Unit-level)
message("Saving JAGS hierarchical abundance model as .txt file.")

    cat("
          model {

          # Noninformative Prior distributions
          p0 ~ dunif(0,1)
          alpha0 <- logit(p0)
          beta0 ~ dnorm(0,1)

          for(n in 1:nVisits){
          alpha1[n] ~ dnorm(0, 1) #fixed effects of visit on det
          }

          for(h in 1:nYears){
          beta1[h] ~ dnorm(0, 1) #fixed effects of year on abun
          }

          for(g in 1:nUnits){
          beta2[g] ~ dnorm(0, 1) #fixed effects of SMI_unit on abun
          gamma[g]~dunif(0,5)

          }

          for(i in 1:M){ # Loop over sites
          # Conditional multinomial cell probabilities
          pi[i,1] <- p[i]
          pi[i,2] <- p[i]*(1-p[i])
          pi[i,3] <- p[i]*(1-p[i])*(1-p[i])
          pi[i,4] <- p[i]*(1-p[i])*(1-p[i])*(1-p[i])
          pi[i,5] <- p[i]*(1-p[i])*(1-p[i])*(1-p[i])*(1-p[i])
          pi0[i] <- 1 - (pi[i,1] + pi[i,2] + pi[i,3] + pi[i,4] + pi[i,5])
          pcap[i] <- 1 - pi0[i]
          for(j in 1:5){
          pic[i,j] <- pi[i,j] / pcap[i]
          }

          # logit-linear model for detection: with visit and observer effects
          logit(p[i]) <- alpha0 + alpha1[visit[i]]

          #Abundance model for Year 1
          log(lambda[i])<- beta0  + beta2[unit[i]]

          y[i,1] ~ dbin(pcap[i], N[i,1])
          N[i,1] ~ dpois(lambda[i])

          #Abundance for years with declining trend
          for (t in 2:nYears){
          y[i,t] ~ dbin(pcap[i], N[i,t])
          N[i,t]~ dpois(N[i, t-1]*gamma[unit[i]])
          }#t
          }#i


          }
          ",fill=TRUE, file="JAGS.model.unit.txt")

}

#End