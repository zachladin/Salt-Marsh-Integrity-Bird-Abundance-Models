#createFolders
createFolders<-function(dataIn){

  new.data<-dataIn

###########################################################################################
#generate required lists

#speciesList
speciesList<-sort(c("CLRA","NESP","SALS","SESP","TMO","WILL"))

#NWR_code list
refugeList<-sort(unique(data$NWR_code))

#unitList
unitList<-sort(unique(data$SMI_Unit))

###########################################################################################
#create data folders

#data filepaths
dataFilepath<-paste(getwd(),"Data",sep="/")
refugeDataFilepath_full<-paste(getwd(),"Data","Data_refuge_full",sep="/")
refugeDataFilepath_half_A<-paste(getwd(),"Data","Data_refuge_half_A",sep="/")
refugeDataFilepath_half_B<-paste(getwd(),"Data","Data_refuge_half_B",sep="/")
refugeDataFilepath_third_A<-paste(getwd(),"Data","Data_refuge_third_A",sep="/")
refugeDataFilepath_third_B<-paste(getwd(),"Data","Data_refuge_third_B",sep="/")
refugeDataFilepath_third_C<-paste(getwd(),"Data","Data_refuge_third_C",sep="/")

speciesUMFdataFilepath<-paste(getwd(),"Data","Data_spp_umf",sep="/")

speciesUMFdataFilepath_full<-paste(getwd(),"Data","Data_spp_umf_full",sep="/")
speciesUMFdataFilepath_half_A<-paste(getwd(),"Data","Data_spp_umf_half_A",sep="/")
speciesUMFdataFilepath_half_B<-paste(getwd(),"Data","Data_spp_umf_half_B",sep="/")
speciesUMFdataFilepath_third_A<-paste(getwd(),"Data","Data_spp_umf_third_A",sep="/")
speciesUMFdataFilepath_third_B<-paste(getwd(),"Data","Data_spp_umf_third_B",sep="/")
speciesUMFdataFilepath_third_C<-paste(getwd(),"Data","Data_spp_umf_third_C",sep="/")

#data folders
dir.create(path=paste(getwd(),"Data","Data_Half",sep="/"))
dir.create(path=paste(getwd(),"Data","Data_Thirds",sep="/"))
dir.create(path=dataFilepath)

dir.create(path=refugeDataFilepath_full)
dir.create(path=refugeDataFilepath_half_A)
dir.create(path=refugeDataFilepath_half_B)
dir.create(path=refugeDataFilepath_third_A)
dir.create(path=refugeDataFilepath_third_B)
dir.create(path=refugeDataFilepath_third_C)

dir.create(path=speciesUMFdataFilepath)

dir.create(path=speciesUMFdataFilepath_full)
dir.create(path=speciesUMFdataFilepath_half_A)
dir.create(path=speciesUMFdataFilepath_half_B)
dir.create(path=speciesUMFdataFilepath_third_A)
dir.create(path=speciesUMFdataFilepath_third_B)
dir.create(path=speciesUMFdataFilepath_third_C)

###########################################################################################
#create Results folders
resultsFilepath<-paste(getwd(),"Results",sep="/")
dir.create(path=resultsFilepath)

###########################################################################################
#SMI Full model results

#Full - filepaths
#annual
SMIfullFilepath<-paste(getwd(), "Results","SMI_full",sep="/")
  SMIfullFilepath_annual<-paste(SMIfullFilepath, "SMI_full_annual",sep="/")
    refugeFilepath_full_annual<-paste(SMIfullFilepath_annual,"Abundance_refuge",sep="/")
    bayesPwrFilepath_full_annual<-paste(SMIfullFilepath_annual,"Power_Bayes",sep="/")
#biennial
  SMIfullFilepath_biennial<-paste(SMIfullFilepath, "SMI_full_biennial",sep="/")
    refugeFilepath_full_biennial<-paste(SMIfullFilepath_biennial,"Abundance_refuge",sep="/")
    bayesPwrFilepath_full_biennial<-paste(SMIfullFilepath_biennial,"Power_Bayes",sep="/")
#triennial
  SMIfullFilepath_triennial<-paste(SMIfullFilepath, "SMI_full_triennial",sep="/")
    refugeFilepath_full_triennial<-paste(SMIfullFilepath_triennial,"Abundance_refuge",sep="/")
    bayesPwrFilepath_full_triennial<-paste(SMIfullFilepath_triennial,"Power_Bayes",sep="/")

#Full - create folders
dir.create(path=SMIfullFilepath)
dir.create(path=SMIfullFilepath_annual)
dir.create(path=SMIfullFilepath_biennial)
dir.create(path=SMIfullFilepath_triennial)

dir.create(path=refugeFilepath_full_annual)
dir.create(path=refugeFilepath_full_biennial)
dir.create(path=refugeFilepath_full_triennial)

dir.create(path=bayesPwrFilepath_full_annual)
dir.create(path=bayesPwrFilepath_full_biennial)
dir.create(path=bayesPwrFilepath_full_triennial)


#create all subfolders for NWR_code
for(j in 1:length(speciesList)){
  for (i in 1:length(refugeList)){
    dir.create(paste(refugeFilepath_full_annual,speciesList[j],sep="/"))
    dir.create(paste(refugeFilepath_full_annual,speciesList[j], refugeList[i],sep="/"))

    dir.create(paste(refugeFilepath_full_biennial,speciesList[j],sep="/"))
    dir.create(paste(refugeFilepath_full_biennial,speciesList[j], refugeList[i],sep="/"))

    dir.create(paste(refugeFilepath_full_triennial,speciesList[j],sep="/"))
    dir.create(paste(refugeFilepath_full_triennial,speciesList[j], refugeList[i],sep="/"))
  }
}

###########################################################################################
#SMI Half_A model results

#Half - filepaths
#annual
SMIhalf_AFilepath<-paste(getwd(), "Results","SMI_half_A",sep="/")
SMIhalf_AFilepath_annual<-paste(SMIhalf_AFilepath, "SMI_half_A_annual",sep="/")
refugeFilepath_half_A_annual<-paste(SMIhalf_AFilepath_annual,"Abundance_refuge",sep="/")
bayesPwrFilepath_half_A_annual<-paste(SMIhalf_AFilepath_annual,"Power_Bayes",sep="/")
#biennial
SMIhalf_AFilepath_biennial<-paste(SMIhalf_AFilepath, "SMI_half_A_biennial",sep="/")
refugeFilepath_half_A_biennial<-paste(SMIhalf_AFilepath_biennial,"Abundance_refuge",sep="/")
bayesPwrFilepath_half_A_biennial<-paste(SMIhalf_AFilepath_biennial,"Power_Bayes",sep="/")
#triennial
SMIhalf_AFilepath_triennial<-paste(SMIhalf_AFilepath, "SMI_half_A_triennial",sep="/")
refugeFilepath_half_A_triennial<-paste(SMIhalf_AFilepath_triennial,"Abundance_refuge",sep="/")
bayesPwrFilepath_half_A_triennial<-paste(SMIhalf_AFilepath_triennial,"Power_Bayes",sep="/")

#Half_A - create folders
dir.create(path=SMIhalf_AFilepath)
dir.create(path=SMIhalf_AFilepath_annual)
dir.create(path=SMIhalf_AFilepath_biennial)
dir.create(path=SMIhalf_AFilepath_triennial)

dir.create(path=refugeFilepath_half_A_annual)
dir.create(path=refugeFilepath_half_A_biennial)
dir.create(path=refugeFilepath_half_A_triennial)

dir.create(path=bayesPwrFilepath_half_A_annual)
dir.create(path=bayesPwrFilepath_half_A_biennial)
dir.create(path=bayesPwrFilepath_half_A_triennial)


#create all subfolders for NWR_code
for(j in 1:length(speciesList)){
  for (i in 1:length(refugeList)){
    dir.create(paste(refugeFilepath_half_A_annual,speciesList[j],sep="/"))
    dir.create(paste(refugeFilepath_half_A_annual,speciesList[j], refugeList[i],sep="/"))

    dir.create(paste(refugeFilepath_half_A_biennial,speciesList[j],sep="/"))
    dir.create(paste(refugeFilepath_half_A_biennial,speciesList[j], refugeList[i],sep="/"))

    dir.create(paste(refugeFilepath_half_A_triennial,speciesList[j],sep="/"))
    dir.create(paste(refugeFilepath_half_A_triennial,speciesList[j], refugeList[i],sep="/"))
  }
}

###########################################################################################
#SMI Half_B model results

#Half - filepaths
#annual
SMIhalf_BFilepath<-paste(getwd(), "Results","SMI_half_B",sep="/")
SMIhalf_BFilepath_annual<-paste(SMIhalf_BFilepath, "SMI_half_B_annual",sep="/")
refugeFilepath_half_B_annual<-paste(SMIhalf_BFilepath_annual,"Abundance_refuge",sep="/")
bayesPwrFilepath_half_B_annual<-paste(SMIhalf_BFilepath_annual,"Power_Bayes",sep="/")
#biennial
SMIhalf_BFilepath_biennial<-paste(SMIhalf_BFilepath, "SMI_half_B_biennial",sep="/")
refugeFilepath_half_B_biennial<-paste(SMIhalf_BFilepath_biennial,"Abundance_refuge",sep="/")
bayesPwrFilepath_half_B_biennial<-paste(SMIhalf_BFilepath_biennial,"Power_Bayes",sep="/")
#triennial
SMIhalf_BFilepath_triennial<-paste(SMIhalf_BFilepath, "SMI_half_B_triennial",sep="/")
refugeFilepath_half_B_triennial<-paste(SMIhalf_BFilepath_triennial,"Abundance_refuge",sep="/")
bayesPwrFilepath_half_B_triennial<-paste(SMIhalf_BFilepath_triennial,"Power_Bayes",sep="/")

#Half_B - create folders
dir.create(path=SMIhalf_BFilepath)
dir.create(path=SMIhalf_BFilepath_annual)
dir.create(path=SMIhalf_BFilepath_biennial)
dir.create(path=SMIhalf_BFilepath_triennial)

dir.create(path=refugeFilepath_half_B_annual)
dir.create(path=refugeFilepath_half_B_biennial)
dir.create(path=refugeFilepath_half_B_triennial)

dir.create(path=bayesPwrFilepath_half_B_annual)
dir.create(path=bayesPwrFilepath_half_B_biennial)
dir.create(path=bayesPwrFilepath_half_B_triennial)


#create all subfolders for NWR_code
for(j in 1:length(speciesList)){
  for (i in 1:length(refugeList)){
    dir.create(paste(refugeFilepath_half_B_annual,speciesList[j],sep="/"))
    dir.create(paste(refugeFilepath_half_B_annual,speciesList[j], refugeList[i],sep="/"))

    dir.create(paste(refugeFilepath_half_B_biennial,speciesList[j],sep="/"))
    dir.create(paste(refugeFilepath_half_B_biennial,speciesList[j], refugeList[i],sep="/"))

    dir.create(paste(refugeFilepath_half_B_triennial,speciesList[j],sep="/"))
    dir.create(paste(refugeFilepath_half_B_triennial,speciesList[j], refugeList[i],sep="/"))
  }
}


###########################################################################################
#SMI Third_A model results

#Third_A - filepaths
#annual
SMIthird_AFilepath<-paste(getwd(), "Results","SMI_third_A",sep="/")
SMIthird_AFilepath_annual<-paste(SMIthird_AFilepath, "SMI_third_A_annual",sep="/")
refugeFilepath_third_A_annual<-paste(SMIthird_AFilepath_annual,"Abundance_refuge",sep="/")
bayesPwrFilepath_third_A_annual<-paste(SMIthird_AFilepath_annual,"Power_Bayes",sep="/")
#biennial
SMIthird_AFilepath_biennial<-paste(SMIthird_AFilepath, "SMI_third_A_biennial",sep="/")
refugeFilepath_third_A_biennial<-paste(SMIthird_AFilepath_biennial,"Abundance_refuge",sep="/")
bayesPwrFilepath_third_A_biennial<-paste(SMIthird_AFilepath_biennial,"Power_Bayes",sep="/")
#triennial
SMIthird_AFilepath_triennial<-paste(SMIthird_AFilepath, "SMI_third_A_triennial",sep="/")
refugeFilepath_third_A_triennial<-paste(SMIthird_AFilepath_triennial,"Abundance_refuge",sep="/")
bayesPwrFilepath_third_A_triennial<-paste(SMIthird_AFilepath_triennial,"Power_Bayes",sep="/")

#Third_A- create folders
dir.create(path=SMIthird_AFilepath)
dir.create(path=SMIthird_AFilepath_annual)
dir.create(path=SMIthird_AFilepath_biennial)
dir.create(path=SMIthird_AFilepath_triennial)

dir.create(path=refugeFilepath_third_A_annual)
dir.create(path=refugeFilepath_third_A_biennial)
dir.create(path=refugeFilepath_third_A_triennial)

dir.create(path=bayesPwrFilepath_third_A_annual)
dir.create(path=bayesPwrFilepath_third_A_biennial)
dir.create(path=bayesPwrFilepath_third_A_triennial)


#create all subfolders for NWR_code
for(j in 1:length(speciesList)){
  for (i in 1:length(refugeList)){
    dir.create(paste(refugeFilepath_third_A_annual,speciesList[j],sep="/"))
    dir.create(paste(refugeFilepath_third_A_annual,speciesList[j], refugeList[i],sep="/"))

    dir.create(paste(refugeFilepath_third_A_biennial,speciesList[j],sep="/"))
    dir.create(paste(refugeFilepath_third_A_biennial,speciesList[j], refugeList[i],sep="/"))

    dir.create(paste(refugeFilepath_third_A_triennial,speciesList[j],sep="/"))
    dir.create(paste(refugeFilepath_third_A_triennial,speciesList[j], refugeList[i],sep="/"))
  }
}

###########################################################################################
#SMI Third_B model results

#Third_B - filepaths
#annual
SMIthird_BFilepath<-paste(getwd(), "Results","SMI_third_B",sep="/")
SMIthird_BFilepath_annual<-paste(SMIthird_BFilepath, "SMI_third_B_annual",sep="/")
refugeFilepath_third_B_annual<-paste(SMIthird_BFilepath_annual,"Abundance_refuge",sep="/")
bayesPwrFilepath_third_B_annual<-paste(SMIthird_BFilepath_annual,"Power_Bayes",sep="/")
#biennial
SMIthird_BFilepath_biennial<-paste(SMIthird_BFilepath, "SMI_third_B_biennial",sep="/")
refugeFilepath_third_B_biennial<-paste(SMIthird_BFilepath_biennial,"Abundance_refuge",sep="/")
bayesPwrFilepath_third_B_biennial<-paste(SMIthird_BFilepath_biennial,"Power_Bayes",sep="/")
#triennial
SMIthird_BFilepath_triennial<-paste(SMIthird_BFilepath, "SMI_third_B_triennial",sep="/")
refugeFilepath_third_B_triennial<-paste(SMIthird_BFilepath_triennial,"Abundance_refuge",sep="/")
bayesPwrFilepath_third_B_triennial<-paste(SMIthird_BFilepath_triennial,"Power_Bayes",sep="/")

#Half - create folders
dir.create(path=SMIthird_BFilepath)
dir.create(path=SMIthird_BFilepath_annual)
dir.create(path=SMIthird_BFilepath_biennial)
dir.create(path=SMIthird_BFilepath_triennial)

dir.create(path=refugeFilepath_third_B_annual)
dir.create(path=refugeFilepath_third_B_biennial)
dir.create(path=refugeFilepath_third_B_triennial)

dir.create(path=bayesPwrFilepath_third_B_annual)
dir.create(path=bayesPwrFilepath_third_B_biennial)
dir.create(path=bayesPwrFilepath_third_B_triennial)


#create all subfolders for NWR_code
for(j in 1:length(speciesList)){
  for (i in 1:length(refugeList)){
    dir.create(paste(refugeFilepath_third_B_annual,speciesList[j],sep="/"))
    dir.create(paste(refugeFilepath_third_B_annual,speciesList[j], refugeList[i],sep="/"))

    dir.create(paste(refugeFilepath_third_B_biennial,speciesList[j],sep="/"))
    dir.create(paste(refugeFilepath_third_B_biennial,speciesList[j], refugeList[i],sep="/"))

    dir.create(paste(refugeFilepath_third_B_triennial,speciesList[j],sep="/"))
    dir.create(paste(refugeFilepath_third_B_triennial,speciesList[j], refugeList[i],sep="/"))
  }
}

###########################################################################################
#SMI Third_C model results

#Third_C - filepaths
#annual
SMIthird_CFilepath<-paste(getwd(), "Results","SMI_third_C",sep="/")
SMIthird_CFilepath_annual<-paste(SMIthird_CFilepath, "SMI_third_C_annual",sep="/")
refugeFilepath_third_C_annual<-paste(SMIthird_CFilepath_annual,"Abundance_refuge",sep="/")
bayesPwrFilepath_third_C_annual<-paste(SMIthird_CFilepath_annual,"Power_Bayes",sep="/")
#biennial
SMIthird_CFilepath_biennial<-paste(SMIthird_CFilepath, "SMI_third_C_biennial",sep="/")
refugeFilepath_third_C_biennial<-paste(SMIthird_CFilepath_biennial,"Abundance_refuge",sep="/")
bayesPwrFilepath_third_C_biennial<-paste(SMIthird_CFilepath_biennial,"Power_Bayes",sep="/")
#triennial
SMIthird_CFilepath_triennial<-paste(SMIthird_CFilepath, "SMI_third_C_triennial",sep="/")
refugeFilepath_third_C_triennial<-paste(SMIthird_CFilepath_triennial,"Abundance_refuge",sep="/")
bayesPwrFilepath_third_C_triennial<-paste(SMIthird_CFilepath_triennial,"Power_Bayes",sep="/")

#Half - create folders
dir.create(path=SMIthird_CFilepath)
dir.create(path=SMIthird_CFilepath_annual)
dir.create(path=SMIthird_CFilepath_biennial)
dir.create(path=SMIthird_CFilepath_triennial)

dir.create(path=refugeFilepath_third_C_annual)
dir.create(path=refugeFilepath_third_C_biennial)
dir.create(path=refugeFilepath_third_C_triennial)

dir.create(path=bayesPwrFilepath_third_C_annual)
dir.create(path=bayesPwrFilepath_third_C_biennial)
dir.create(path=bayesPwrFilepath_third_C_triennial)


#create all subfolders for NWR_code
for(j in 1:length(speciesList)){
  for (i in 1:length(refugeList)){
    dir.create(paste(refugeFilepath_third_C_annual,speciesList[j],sep="/"))
    dir.create(paste(refugeFilepath_third_C_annual,speciesList[j], refugeList[i],sep="/"))

    dir.create(paste(refugeFilepath_third_C_biennial,speciesList[j],sep="/"))
    dir.create(paste(refugeFilepath_third_C_biennial,speciesList[j], refugeList[i],sep="/"))

    dir.create(paste(refugeFilepath_third_C_triennial,speciesList[j],sep="/"))
    dir.create(paste(refugeFilepath_third_C_triennial,speciesList[j], refugeList[i],sep="/"))
  }
}



}
#End