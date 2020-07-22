#load packages and source files
#function to load required R packages and source files

loadPackagesFunctions<-function(){
  #load R packages
  library(reshape)
  library(plyr)
  library(unmarked)
  library(rjags)
  library(R2WinBUGS)
  library(arm)
  library(ggplot2)
  library(RColorBrewer)
  library(maptools)
  library(spdep)
  library(pwr)
  library(doParallel)
  library(foreach)
  library(dclone)
  
  #load source files with all fuctions
  org.dir<-getwd()
  setwd(paste(getwd(),"R_source",sep="/"))
  
  source("summaryFunction.R")
  source("formatRawData.R")
  source("formatRawDataSharp.R")
  source("makeGMMumf.R")
  source("getStateData.R")
  source("getRefugeData.R")
  #source("getHexagonData.R")#doesn't exist yet
  source("getRegionData.R")
  source("runUnmarked.R")
  source("getModelOutput.R")
  source("getModelOutputRegion.R")
  source("runUnmarkedAllYears.R")
  source("getModelOutputAllYears.R")
  source("getModelOutputAllYearsRegion.R")
  source("runUnmarkedRegion.R")
  source("runUnmarkedAllYearsRegion.R")
  source("getAbundance.R")
  source("getAbundanceRegion.R")
  source("dataSimFullPanel.R")
  source("runJAGSmodelUnits.R")
  source("divideUnitsHalf.R")
  source("divideUnitsThirds.R")
  source("divideUnitsThirdsCore.R")
  source("getCoreUnits.R")
  
  setwd(org.dir)
  dir()
  
}

#End